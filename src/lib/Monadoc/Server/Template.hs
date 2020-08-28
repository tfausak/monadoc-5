module Monadoc.Server.Template where

import qualified Data.Text as Text
import qualified Lucid as H
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import Monadoc.Prelude
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.GitHub.Login as Login
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.User as User

makeHtmlWith
  :: Config.Config -> Maybe User.User -> Text.Text -> H.Html () -> H.Html ()
makeHtmlWith config maybeUser loginUrl content = do
  let route = Router.renderAbsoluteRoute config
  H.doctype_
  H.html_ [H.class_ "bg-near-white black sans-serif", H.lang_ "en-US"] $ do
    H.head_ $ do
      H.meta_ [H.charset_ "utf-8"]
      H.meta_
        [ H.name_ "description"
        , H.content_ "\x1f516 Better Haskell documentation."
        ]
      H.meta_
        [H.name_ "viewport", H.content_ "initial-scale=1,width=device-width"]
      let og k v = H.meta_ [H.term "property" $ "og:" <> k, H.content_ v]
      og "title" "Monadoc"
      og "type" "website"
      let url = route Route.Index
      og "url" url
      og "image" $ route Route.Logo
      H.link_ [H.rel_ "canonical", H.href_ url]
      H.link_ [H.rel_ "icon", H.href_ $ route Route.Favicon]
      H.link_ [H.rel_ "apple-touch-icon", H.href_ $ route Route.Logo]
      H.link_ [H.rel_ "stylesheet", H.href_ $ route Route.Tachyons]
      H.title_ "Monadoc"
    H.body_ $ do
      H.header_ [H.class_ "bg-purple white"]
        <<< H.div_ [H.class_ "center mw8 pa3"]
        $ do
            H.div_ [H.class_ "flex items-center justify-between"] $ do
              H.h1_ [H.class_ "f2 lh-solid ma0 tracked-tight"] $ H.a_
                [ H.class_ "color-inherit no-underline"
                , H.href_ $ route Route.Index
                ]
                "Monadoc"
              case maybeUser of
                Nothing -> H.a_
                  [H.class_ "color-inherit no-underline", H.href_ loginUrl]
                  "Log in with GitHub"
                Just user ->
                  H.a_
                      [ H.class_ "color-inherit no-underline"
                      , H.href_ $ route Route.Account
                      ]
                    <<< H.toHtml
                    <<< Text.cons '@'
                    <<< Login.toText
                    $ User.login user
      H.div_ [H.class_ "center mw8 pa3"]
        <<< H.form_
              [ H.action_ $ route Route.Search
              , H.class_ "b--inherit ba bg-white flex items-center"
              ]
        $ do
            H.input_
              [ H.class_ "bn pa2 w-100"
              , H.name_ "query"
              , H.placeholder_ "Search for something ..."
              ]
            H.input_
              [ H.class_ "b bg-inherit bn pa2 pointer"
              , H.type_ "submit"
              , H.value_ "Search"
              ]
      H.main_ [H.class_ "bg-white"]
        $ H.div_ [H.class_ "center mw8 pa3"] content
      H.footer_ [H.class_ "center mid-gray mw8 pa3 tc"] $ do
        "Powered by "
        H.a_
          [ H.class_ "color-inherit"
          , H.href_ "https://github.com/tfausak/monadoc"
          ]
          "Monadoc"
        " version "
        H.code_ $ H.toHtml Version.string
        case Commit.hash of
          Nothing -> pure ()
          Just commit -> do
            " commit "
            H.code_ <<< H.toHtml $ take 7 commit
        "."
