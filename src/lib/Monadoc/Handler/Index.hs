module Monadoc.Handler.Index
  ( handle
  )
where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.UUID as Uuid
import qualified Lucid
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
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cookie

handle :: App.App Wai.Request Wai.Response
handle = do
  context <- Reader.ask
  maybeUser <-
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

  let config = Context.config context
  loginUrl <- makeLoginUrl
  pure . Common.htmlResponse Http.ok200 (Common.defaultHeaders config) $ do
    Lucid.doctype_
    Lucid.html_ [Lucid.lang_ "en-US"] $ do
      Lucid.head_ $ do
        Lucid.meta_ [Lucid.charset_ "utf-8"]
        Lucid.meta_
          [ Lucid.name_ "description"
          , Lucid.content_ "\x1f516 Better Haskell documentation."
          ]
        Lucid.meta_
          [ Lucid.name_ "viewport"
          , Lucid.content_ "initial-scale=1,width=device-width"
          ]
        let
          og property content =
            Lucid.meta_
              [ Lucid.term "property" $ "og:" <> property
              , Lucid.content_ content
              ]
        og "title" "Monadoc"
        og "type" "website"
        let url = Router.renderAbsoluteRoute config Route.Index
        og "url" url
        og "image" $ Router.renderAbsoluteRoute config Route.Logo
        Lucid.link_ [Lucid.rel_ "canonical", Lucid.href_ url]
        Lucid.link_
          [ Lucid.rel_ "icon"
          , Lucid.href_ $ Router.renderRelativeRoute Route.Favicon
          ]
        Lucid.link_
          [ Lucid.rel_ "apple-touch-icon"
          , Lucid.href_ $ Router.renderRelativeRoute Route.Logo
          ]
        Lucid.link_
          [ Lucid.rel_ "stylesheet"
          , Lucid.href_ $ Router.renderRelativeRoute Route.Tachyons
          ]
        Lucid.title_ "Monadoc"
      Lucid.body_ [Lucid.class_ "bg-white black sans-serif"] $ do
        Lucid.header_
            [ Lucid.class_
                "bg-purple flex items-center justify-between pa3 white"
            ]
          $ do
              Lucid.h1_ [Lucid.class_ "ma0 normal"] $ Lucid.a_
                [ Lucid.class_ "color-inherit no-underline"
                , Lucid.href_ $ Router.renderRelativeRoute Route.Index
                ]
                "Monadoc"
              case maybeUser of
                Nothing -> Lucid.a_
                  [ Lucid.class_ "color-inherit no-underline"
                  , Lucid.href_ loginUrl
                  ]
                  "Log in with GitHub"
                Just user -> do
                  "@"
                  Lucid.toHtml . Login.toText $ User.login user
        Lucid.main_ [Lucid.class_ "pa3"]
          $ Lucid.p_ "\x1f516 Better Haskell documentation."
        Lucid.footer_ [Lucid.class_ "mid-gray pa3 tc"] . Lucid.p_ $ do
          "Powered by "
          Lucid.a_
            [ Lucid.class_ "color-inherit"
            , Lucid.href_ "https://github.com/tfausak/monadoc"
            ]
            "Monadoc"
          " version "
          Lucid.code_ $ Lucid.toHtml Version.string
          case Commit.hash of
            Nothing -> pure ()
            Just commit -> do
              " commit "
              Lucid.code_ . Lucid.toHtml $ take 7 commit
          "."

-- TODO: Include `state`.
-- TODO: Redirect back to current URL.
makeLoginUrl :: App.App Wai.Request Text.Text
makeLoginUrl = do
  context <- Reader.ask
  let
    config = Context.config context
    query = Http.renderQueryText
      True
      [ ("client_id", Just . Text.pack $ Config.clientId config)
      , ( "redirect_uri"
        , Just $ Router.renderAbsoluteRoute config Route.GitHubCallback
        )
      ]
  pure
    . Text.decodeUtf8With Text.lenientDecode
    . LazyByteString.toStrict
    . Builder.toLazyByteString
    $ "https://github.com/login/oauth/authorize"
    <> query
