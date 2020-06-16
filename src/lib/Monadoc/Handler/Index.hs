module Monadoc.Handler.Index
  ( handle
  -- TODO: Move these somewhere else.
  , getCookieUser
  , makeHtmlWith
  , makeLoginUrl
  )
where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.UUID as Uuid
import qualified Lucid as Html
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.GitHub.Login as Login
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.User as User
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cookie

handle :: App.App Wai.Request Wai.Response
handle = do
  context <- Reader.ask
  maybeUser <- getCookieUser

  let config = Context.config context
  loginUrl <- makeLoginUrl
  pure
    . Common.htmlResponse Http.ok200 (Common.defaultHeaders config)
    . makeHtmlWith config maybeUser loginUrl
    $ Html.p_ "\x1f516 Better Haskell documentation."

getCookieUser :: App.App Wai.Request (Maybe User.User)
getCookieUser = do
  context <- Reader.ask
  case lookup Http.hCookie . Wai.requestHeaders $ Context.request context of
    Nothing -> pure Nothing
    Just cookie -> case lookup "guid" $ Cookie.parseCookiesText cookie of
      Nothing -> pure Nothing
      Just text -> case Guid.fromUuid <$> Uuid.fromText text of
        Nothing -> pure Nothing
        Just guid ->
          fmap Maybe.listToMaybe . App.withConnection $ \connection ->
            Trans.lift $ Sql.query
              connection
              "select * from users where guid = ?"
              [guid]

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

makeHtmlWith
  :: Config.Config
  -> Maybe User.User
  -> Text.Text
  -> Html.Html ()
  -> Html.Html ()
makeHtmlWith config maybeUser loginUrl content = do
  Html.doctype_
  Html.html_ [Html.lang_ "en-US"] $ do
    Html.head_ $ do
      Html.meta_ [Html.charset_ "utf-8"]
      Html.meta_
        [ Html.name_ "description"
        , Html.content_ "\x1f516 Better Haskell documentation."
        ]
      Html.meta_
        [ Html.name_ "viewport"
        , Html.content_ "initial-scale=1,width=device-width"
        ]
      let
        og k v =
          Html.meta_ [Html.term "property" $ "og:" <> k, Html.content_ v]
      og "title" "Monadoc"
      og "type" "website"
      let url = Router.renderAbsoluteRoute config Route.Index
      og "url" url
      og "image" $ Router.renderAbsoluteRoute config Route.Logo
      Html.link_ [Html.rel_ "canonical", Html.href_ url]
      Html.link_
        [ Html.rel_ "icon"
        , Html.href_ $ Router.renderAbsoluteRoute config Route.Favicon
        ]
      Html.link_
        [ Html.rel_ "apple-touch-icon"
        , Html.href_ $ Router.renderAbsoluteRoute config Route.Logo
        ]
      Html.link_
        [ Html.rel_ "stylesheet"
        , Html.href_ $ Router.renderAbsoluteRoute config Route.Tachyons
        ]
      Html.title_ "Monadoc"
    Html.body_ [Html.class_ "bg-white black sans-serif"] $ do
      Html.header_
          [Html.class_ "bg-purple flex items-center justify-between pa3 white"]
        $ do
            Html.h1_ [Html.class_ "ma0 normal"] $ Html.a_
              [ Html.class_ "color-inherit no-underline"
              , Html.href_ $ Router.renderAbsoluteRoute config Route.Index
              ]
              "Monadoc"
            Html.p_ $ case maybeUser of
              Nothing -> Html.a_
                [Html.class_ "color-inherit no-underline", Html.href_ loginUrl]
                "Log in with GitHub"
              Just user ->
                Html.a_
                    [ Html.class_ "color-inherit no-underline"
                    , Html.href_
                      $ Router.renderAbsoluteRoute config Route.Account
                    ]
                  $ do
                      "@"
                      Html.toHtml . Login.toText $ User.login user
      Html.main_ [Html.class_ "pa3"] content
      Html.footer_ [Html.class_ "mid-gray pa3 tc"] . Html.p_ $ do
        "Powered by "
        Html.a_
          [ Html.class_ "color-inherit"
          , Html.href_ "https://github.com/tfausak/monadoc"
          ]
          "Monadoc"
        " version "
        Html.code_ $ Html.toHtml Version.string
        case Commit.hash of
          Nothing -> pure ()
          Just commit -> do
            " commit "
            Html.code_ . Html.toHtml $ take 7 commit
        "."
