module Monadoc.Server.Settings where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Proxy as Proxy
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import Monadoc.Prelude
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.NotFoundException as NotFoundException
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Utility.Console as Console
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- | Builds Warp server settings from a context.
fromContext :: Context.Context request -> Warp.Settings
fromContext context =
  let config = Context.config context
  in
    Warp.setBeforeMainLoop (beforeMainLoop config)
    <<< Warp.setHost (Config.host config)
    <<< Warp.setOnException (onException context)
    <<< Warp.setOnExceptionResponse (onExceptionResponse config)
    <<< Warp.setPort (Config.port config)
    <| Warp.setServerName serverName Warp.defaultSettings

beforeMainLoop :: Config.Config -> IO ()
beforeMainLoop config = Console.info <| unwords
  [ "Listening on"
  , show <| Config.host config
  , "port"
  , show <| Config.port config
  , "..."
  ]

onException
  :: Context.Context request
  -> Maybe Wai.Request
  -> Exception.SomeException
  -> IO ()
onException context _ exception
  | not <| Warp.defaultShouldDisplayException exception = pure ()
  | isType notFoundException exception = pure ()
  | isType testException exception = pure ()
  | otherwise = do
    Console.warn <| Exception.displayException exception
    sendExceptionToDiscord context exception

sendExceptionToDiscord
  :: Context.Context request -> Exception.SomeException -> IO ()
sendExceptionToDiscord context exception =
  case Client.parseRequest <<< Config.discordUrl <| Context.config context of
    Left someException -> case Exception.fromException someException of
      Just (Client.InvalidUrlException url reason) ->
        Console.warn <| "invalid Discord URL (" <> reason <> "): " <> show url
      _ -> Exception.throwM someException
    Right initialRequest -> do
      let
        content = Utf8.fromString
          <| fold ["```\n", Exception.displayException exception, "```"]
        request = Client.urlEncodedBody [("content", content)] initialRequest
        manager = Context.manager context
      Monad.void <| Client.httpLbs request manager

onExceptionResponse :: Config.Config -> Exception.SomeException -> Wai.Response
onExceptionResponse config exception
  | isType notFoundException exception = Common.statusResponse
    Http.notFound404
    headers
  | otherwise = Common.statusResponse Http.internalServerError500 headers
  where headers = Common.defaultHeaders config

notFoundException :: Proxy.Proxy NotFoundException.NotFoundException
notFoundException = Proxy.Proxy

testException :: Proxy.Proxy TestException.TestException
testException = Proxy.Proxy

isType
  :: Exception.Exception e => Proxy.Proxy e -> Exception.SomeException -> Bool
isType proxy =
  maybe False (always True <<< asType proxy)
    <<< Exception.fromException
    <<< WithCallStack.withoutCallStack

asType :: Proxy.Proxy a -> a -> a
asType _ = id

serverName :: ByteString.ByteString
serverName =
  Utf8.fromString <| "monadoc-" <> Version.string <> case Commit.hash of
    Nothing -> ""
    Just hash -> "-" <> hash
