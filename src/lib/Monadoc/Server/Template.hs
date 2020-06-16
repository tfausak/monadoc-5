module Monadoc.Server.Template
  ( makeHtmlWith
  )
where

import qualified Data.Text as Text
import qualified Lucid as Html
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.GitHub.Login as Login
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.User as User

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
