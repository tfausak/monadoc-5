module Monadoc.Server.Settings
  ( fromConfig
  , onException
  , onExceptionResponse
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Monadoc.Console as Console
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- | Builds Warp server settings from a config.
fromConfig :: Config.Config -> Warp.Settings
fromConfig config =
  Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (Config.host config)
    . Warp.setOnException onException
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

onException :: Maybe Wai.Request -> Exception.SomeException -> IO ()
onException _ someException@(Exception.SomeException exception) =
  Monad.when (Warp.defaultShouldDisplayException someException)
    . Console.warn
    $ Exception.displayException exception

onExceptionResponse :: Config.Config -> Exception.SomeException -> Wai.Response
onExceptionResponse config _ =
  Common.statusResponse Http.internalServerError500
    $ Common.defaultHeaders config

serverName :: ByteString.ByteString
serverName =
  Utf8.fromString $ "monadoc-" <> Version.string <> case Commit.hash of
    Nothing -> ""
    Just hash -> "-" <> hash
