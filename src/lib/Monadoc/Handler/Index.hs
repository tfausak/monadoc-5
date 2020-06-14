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
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

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
              Lucid.a_
                [ Lucid.class_ "color-inherit no-underline"
                , Lucid.href_ . Text.pack $ mconcat
                  [ "http://github.com/login/oauth/authorize?client_id="
                  , Config.clientId config
                  , "&redirect_uri="
                  , Config.url config
                  , "/todo"
                  ]
                ]
                "Log in with GitHub"
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
