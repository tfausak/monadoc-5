module Monadoc.Handler.Index
  ( handle
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Text as Text
import qualified Lucid
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

renderAbsoluteRoute :: Config.Config -> Route.Route -> Text.Text
renderAbsoluteRoute config =
  mappend (Text.pack $ Config.url config) . renderRelativeRoute

renderRelativeRoute :: Route.Route -> Text.Text
renderRelativeRoute route = case route of
  Route.Favicon -> "/favicon.ico"
  Route.HealthCheck -> "/health-check"
  Route.Index -> "/"
  Route.Logo -> "/static/logo.png"
  Route.Robots -> "/robots.txt"
  Route.Tachyons -> "/static/tachyons-4-12-0.css"
  Route.Throw -> "/throw"

handle :: App.App request Wai.Response
handle = do
  config <- Reader.asks Context.config
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
        let url = renderAbsoluteRoute config Route.Index
        og "url" url
        og "image" $ renderAbsoluteRoute config Route.Logo
        Lucid.link_ [Lucid.rel_ "canonical", Lucid.href_ url]
        Lucid.link_
          [Lucid.rel_ "icon", Lucid.href_ $ renderRelativeRoute Route.Favicon]
        Lucid.link_
          [ Lucid.rel_ "apple-touch-icon"
          , Lucid.href_ $ renderRelativeRoute Route.Logo
          ]
        Lucid.link_
          [ Lucid.rel_ "stylesheet"
          , Lucid.href_ $ renderRelativeRoute Route.Tachyons
          ]
        Lucid.title_ "Monadoc"
      Lucid.body_ [Lucid.class_ "bg-white black sans-serif"] $ do
        Lucid.header_ [Lucid.class_ "bg-purple pa3 white"]
          . Lucid.h1_ [Lucid.class_ "ma0 normal"]
          $ Lucid.a_
              [ Lucid.class_ "color-inherit no-underline"
              , Lucid.href_ $ renderRelativeRoute Route.Index
              ]
              "Monadoc"
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
