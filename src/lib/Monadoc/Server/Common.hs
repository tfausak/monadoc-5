module Monadoc.Server.Common
  ( Headers
  , byteStringResponse
  , defaultHeaders
  , fileResponse
  , htmlResponse
  , isSecure
  , simpleFileResponse
  , statusResponse
  , stringResponse
  )
where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Lucid as Html
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Package
import qualified System.FilePath as FilePath

type Headers = Map.Map Http.HeaderName ByteString.ByteString

byteStringResponse
  :: Http.Status -> Headers -> ByteString.ByteString -> Wai.Response
byteStringResponse status headers body =
  let
    contentLength = Utf8.fromString . show $ ByteString.length body
    etag = Utf8.fromString . show . show $ Crypto.hashWith Crypto.SHA256 body
    extraHeaders =
      Map.fromList [(Http.hContentLength, contentLength), (Http.hETag, etag)]
  in Wai.responseLBS
    status
    (Map.toList $ Map.union extraHeaders headers)
    (LazyByteString.fromStrict body)

defaultHeaders :: Config.Config -> Headers
defaultHeaders config =
  let
    contentSecurityPolicy = "base-uri 'none'; default-src 'self'"
    strictTransportSecurity =
      "max-age=" <> if isSecure config then "86400" else "0"
  in Map.fromList
    [ ("Content-Security-Policy", contentSecurityPolicy)
    , ("Referrer-Policy", "no-referrer")
    , ("Strict-Transport-Security", strictTransportSecurity)
    , ("X-Content-Type-Options", "nosniff")
    , ("X-Frame-Options", "deny")
    ]

fileResponse
  :: Http.Status -> Headers -> FilePath -> App.App request Wai.Response
fileResponse status headers name = Trans.lift $ do
  let relative = FilePath.combine "data" name
  absolute <- Package.getDataFileName relative
  contents <- ByteString.readFile absolute
  pure $ byteStringResponse status headers contents

htmlResponse :: Http.Status -> Headers -> Html.Html a -> Wai.Response
htmlResponse status headers =
  byteStringResponse
      status
      (Map.insert Http.hContentType "text/html;charset=utf-8" headers)
    . LazyByteString.toStrict
    . Html.renderBS

isSecure :: Config.Config -> Bool
isSecure = List.isPrefixOf "https:" . Config.url

simpleFileResponse
  :: FilePath -> ByteString.ByteString -> App.App request Wai.Response
simpleFileResponse file mime = do
  config <- Reader.asks Context.config
  fileResponse
    Http.ok200
    (Map.insert Http.hContentType mime $ defaultHeaders config)
    file

statusResponse :: Http.Status -> Headers -> Wai.Response
statusResponse status headers = stringResponse status headers $ unwords
  [show $ Http.statusCode status, Utf8.toString $ Http.statusMessage status]

stringResponse :: Http.Status -> Headers -> String -> Wai.Response
stringResponse status headers =
  byteStringResponse
      status
      (Map.insert Http.hContentType "text/plain;charset=utf-8" headers)
    . Utf8.fromString
