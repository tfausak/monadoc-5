module Monadoc.Server.Common
  ( Headers
  , defaultHeaders
  , responseBS
  , statusResponse
  , stringResponse
  )
where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai

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
responseBS status headers body =
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
defaultHeaders config = Map.fromList
  [ ("Content-Security-Policy", contentSecurityPolicy)
  , ("Referrer-Policy", "no-referrer")
  , ("Strict-Transport-Security", strictTransportSecurity config)
  , ("X-Content-Type-Options", "nosniff")
  , ("X-Frame-Options", "deny")
  ]

contentSecurityPolicy :: ByteString.ByteString
contentSecurityPolicy = "base-uri 'none', default-src 'self'"

strictTransportSecurity :: Config.Config -> ByteString.ByteString
strictTransportSecurity config =
  if List.isPrefixOf "https:" $ Config.url config then "86400" else "0"
