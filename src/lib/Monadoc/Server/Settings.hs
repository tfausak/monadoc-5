module Monadoc.Server.Settings
  ( fromContext
  , onException
  , onExceptionResponse
  )
where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Monadoc.Console as Console
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
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
    . Warp.setHost (Config.host config)
    . Warp.setOnException (onException context)
    . Warp.setOnExceptionResponse (onExceptionResponse config)
    . Warp.setPort (Config.port config)
    $ Warp.setServerName serverName Warp.defaultSettings

beforeMainLoop :: Config.Config -> IO ()
beforeMainLoop config = Console.info $ unwords
  [ "Listening on"
  , show $ Config.host config
  , "port"
  , show $ Config.port config
  , "..."
  ]

onException
  :: Context.Context request
  -> Maybe Wai.Request
  -> Exception.SomeException
  -> IO ()
onException context _ exception = do
  Console.warn $ Exception.displayException exception
  Monad.when (Warp.defaultShouldDisplayException exception)
    . Monad.void
    . Concurrent.forkIO
    $ sendExceptionToDiscord context exception

sendExceptionToDiscord
  :: Context.Context request -> Exception.SomeException -> IO ()
sendExceptionToDiscord context exception = do
  initialRequest <- Client.parseRequest . Config.discordUrl $ Context.config
    context
  let
    content = Utf8.fromString
      $ mconcat ["```\n", Exception.displayException exception, "```"]
    request = Client.urlEncodedBody [("content", content)] initialRequest
    manager = Context.manager context
  Monad.void $ Client.httpLbs request manager

onExceptionResponse :: Config.Config -> Exception.SomeException -> Wai.Response
onExceptionResponse config _ =
  Common.statusResponse Http.internalServerError500
    $ Common.defaultHeaders config

serverName :: ByteString.ByteString
serverName =
  Utf8.fromString $ "monadoc-" <> Version.string <> case Commit.hash of
    Nothing -> ""
    Just hash -> "-" <> hash
