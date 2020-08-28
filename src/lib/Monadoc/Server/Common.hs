module Monadoc.Server.Common where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.UUID as Uuid
import qualified Lucid as H
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.User as User
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Package
import qualified System.FilePath as FilePath
import qualified Web.Cookie as Cookie
import Monadoc.Prelude

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
    contentSecurityPolicy = Utf8.fromString $ List.intercalate
      "; "
      [ "base-uri 'none'"
      , "default-src 'none'"
      , "form-action 'self'"
      , "frame-ancestors 'none'"
      , "img-src 'self'"
      , "style-src 'self'"
      ]
    strictTransportSecurity =
      let maxAge = if isSecure config then 6 * 31 * 24 * 60 * 60 else 0 :: Int
      in Utf8.fromString $ "max-age=" <> show maxAge
  in Map.fromList
    [ ("Content-Security-Policy", contentSecurityPolicy)
    , ("Feature-Policy", "notifications 'none'")
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

getCookieUser :: App.App Wai.Request (Maybe User.User)
getCookieUser = do
  context <- Reader.ask
  case lookup Http.hCookie . Wai.requestHeaders $ Context.request context of
    Nothing -> pure Nothing
    Just cookie -> case lookup "guid" $ Cookie.parseCookiesText cookie of
      Nothing -> pure Nothing
      Just text -> case fmap Guid.fromUuid $ Uuid.fromText text of
        Nothing -> pure Nothing
        Just guid -> fmap Maybe.listToMaybe
          $ App.sql "select * from users where guid = ?" [guid]

htmlResponse :: Http.Status -> Headers -> H.Html a -> Wai.Response
htmlResponse status headers =
  byteStringResponse
      status
      (Map.insert Http.hContentType "text/html;charset=utf-8" headers)
    . LazyByteString.toStrict
    . H.renderBS

isSecure :: Config.Config -> Bool
isSecure = List.isPrefixOf "https:" . Config.url

makeCookie :: Guid.Guid -> App.App request Cookie.SetCookie
makeCookie guid = do
  config <- Reader.asks Context.config
  pure Cookie.defaultSetCookie
    { Cookie.setCookieHttpOnly = True
    , Cookie.setCookieName = "guid"
    , Cookie.setCookiePath = Just "/"
    , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
    , Cookie.setCookieSecure = isSecure config
    , Cookie.setCookieValue = Uuid.toASCIIBytes $ Guid.toUuid guid
    }

makeLoginUrl :: App.App Wai.Request Text.Text
makeLoginUrl = do
  context <- Reader.ask
  let
    config = Context.config context
    clientId = Text.pack $ Config.clientId config
    route = Router.renderAbsoluteRoute config Route.GitHubCallback
    request = Context.request context
    current = Wai.rawPathInfo request <> Wai.rawQueryString request
    redirectUri = route
      <> Utf8.toText (Http.renderSimpleQuery True [("redirect", current)])
    query = Http.renderQueryText
      True
      [("client_id", Just clientId), ("redirect_uri", Just redirectUri)]
  pure
    . Utf8.toText
    . LazyByteString.toStrict
    . Builder.toLazyByteString
    $ "https://github.com/login/oauth/authorize"
    <> query

renderCookie :: Cookie.SetCookie -> ByteString.ByteString
renderCookie =
  LazyByteString.toStrict . Builder.toLazyByteString . Cookie.renderSetCookie

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
