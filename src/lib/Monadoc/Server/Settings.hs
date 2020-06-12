module Monadoc.Server.Settings
  ( Headers
  , fromConfig
  , onException
  , onExceptionResponse
  , responseBS
  , statusResponse
  , stringResponse
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Monadoc.Console as Console
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- | Builds Warp server settings from a config.
fromConfig :: Config.Config -> Warp.Settings
fromConfig config =
  Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (Config.host config)
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
  , "..."
  ]

onException :: Maybe Wai.Request -> Exception.SomeException -> IO ()
onException _ someException@(Exception.SomeException exception) =
  Monad.when (Warp.defaultShouldDisplayException someException)
    . Console.warn
    $ Exception.displayException exception

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = statusResponse Http.internalServerError500 Map.empty

type Headers = Map.Map Http.HeaderName ByteString.ByteString

statusResponse :: Http.Status -> Headers -> Wai.Response
statusResponse status headers = stringResponse status headers $ unwords
  [show $ Http.statusCode status, Utf8.toString $ Http.statusMessage status]

stringResponse :: Http.Status -> Headers -> String -> Wai.Response
stringResponse status headers =
  responseBS
      status
      (Map.insert Http.hContentType "text/plain;charset=utf-8" headers)
    . Utf8.fromString

responseBS :: Http.Status -> Headers -> ByteString.ByteString -> Wai.Response
responseBS status oldHeaders body =
  let
    contentLength = Utf8.fromString . show $ ByteString.length body
    etag = Utf8.fromString . show . show $ Crypto.hashWith Crypto.SHA256 body
    newHeaders = Map.fromList
      [ (Http.hContentLength, contentLength)
      , ("Content-Security-Policy", "base-uri 'none', default-src 'self'")
      , (Http.hETag, etag)
      , ("Referrer-Policy", "no-referrer")
      , ("X-Content-Type-Options", "nosniff")
      , ("X-Frame-Options", "deny")
      ]
  in Wai.responseLBS
    status
    (Map.toList $ Map.union newHeaders oldHeaders)
    (LazyByteString.fromStrict body)

serverName :: ByteString.ByteString
serverName =
  Utf8.fromString $ "monadoc-" <> Version.string <> case Commit.hash of
    Nothing -> ""
    Just hash -> "-" <> hash
