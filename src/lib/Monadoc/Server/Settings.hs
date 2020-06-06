module Monadoc.Server.Settings
  ( fromConfig
  , onException
  , onExceptionResponse
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Console as Console
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Type.Config as Config
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Monadoc.Utility.Utf8 as Utf8

-- | Builds Warp server settings from a config.
fromConfig :: Config.Config -> Warp.Settings
fromConfig config =
  Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (Config.host config)
    . Warp.setLogger logger
    . Warp.setOnException onException
    . Warp.setOnExceptionResponse onExceptionResponse
    . Warp.setPort (Config.port config)
    $ Warp.setServerName serverName Warp.defaultSettings

beforeMainLoop :: Config.Config -> IO ()
beforeMainLoop config = Console.info $ unwords
  [ "Listening on"
  , show $ Config.host config
  , "port"
  , show $ Config.port config
  ]

logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ = Console.info $ unwords
  [ show $ Http.statusCode status
  , Utf8.toString $ Wai.requestMethod request
  , Utf8.toString $ Wai.rawPathInfo request <> Wai.rawQueryString request
  ]

onException :: Maybe Wai.Request -> Exception.SomeException -> IO ()
onException _ someException@(Exception.SomeException exception) =
  Monad.when (Warp.defaultShouldDisplayException someException)
    . Console.warn
    $ Exception.displayException exception

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = statusResponse Http.internalServerError500 []

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers = stringResponse status headers $ unwords
  [show $ Http.statusCode status, Utf8.toString $ Http.statusMessage status]

stringResponse :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
stringResponse status headers string = Wai.responseLBS
  status
  ((Http.hContentType, Utf8.fromString "text/plain; charset=utf-8") : headers)
  (LazyByteString.fromStrict $ Utf8.fromString string)

serverName :: ByteString.ByteString
serverName =
  Utf8.fromString $ "monadoc-" <> Version.string <> case Commit.hash of
    Nothing -> ""
    Just hash -> "-" <> hash
