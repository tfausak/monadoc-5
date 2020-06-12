module Monadoc.Server.Application
  ( application
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Lucid
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

application :: Context.Context request -> Wai.Application
application context request respond = do
  response <-
    App.run context { Context.request = request } . runRoute $ parseRoute
      request
  respond response

parseRoute :: Wai.Request -> Maybe Route.Route
parseRoute request =
  let
    method = Wai.requestMethod request
    path = Wai.pathInfo request
  in case (method, path) of
    ("GET", []) -> Just Route.Index
    ("GET", ["favicon.ico"]) -> Just Route.Favicon
    ("GET", ["health-check"]) -> Just Route.HealthCheck
    ("GET", ["robots.txt"]) -> Just Route.Robots
    ("GET", ["static", "tachyons-4-12-0.css"]) -> Just Route.Tachyons
    ("GET", ["throw"]) -> Just Route.Throw
    _ -> Nothing

runRoute :: Maybe Route.Route -> App.App Wai.Request Wai.Response
runRoute maybeRoute = case maybeRoute of
  Nothing -> notFoundHandler
  Just route -> case route of
    Route.Index -> rootHandler
    Route.Favicon -> faviconHandler
    Route.HealthCheck -> healthCheckHandler
    Route.Robots -> robotsHandler
    Route.Tachyons -> tachyonsHandler
    Route.Throw -> throwHandler

renderAbsoluteRoute :: Config.Config -> Route.Route -> Text.Text
renderAbsoluteRoute config =
  mappend (Text.pack $ Config.url config) . renderRelativeRoute

renderRelativeRoute :: Route.Route -> Text.Text
renderRelativeRoute route = case route of
  Route.Index -> "/"
  Route.Favicon -> "/favicon.ico"
  Route.HealthCheck -> "/health-check"
  Route.Robots -> "/robots.txt"
  Route.Tachyons -> "/static/tachyons-4-12-0.css"
  Route.Throw -> "/throw"

rootHandler :: App.App request Wai.Response
rootHandler = do
  config <- Reader.asks Context.config
  pure . Common.htmlResponse Http.ok200 (Common.defaultHeaders config) $ do
    Lucid.doctype_
    Lucid.html_ [Lucid.lang_ "en-US"] $ do
      Lucid.head_ $ do
        Lucid.meta_ [Lucid.charset_ "utf-8"]
        Lucid.meta_
          [ Lucid.name_ "description"
          , Lucid.content_ "Better Haskell documentation."
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
        Lucid.link_ [Lucid.rel_ "canonical", Lucid.href_ url]
        Lucid.link_
          [Lucid.rel_ "icon", Lucid.href_ $ renderRelativeRoute Route.Favicon]
        Lucid.link_
          [ Lucid.rel_ "stylesheet"
          , Lucid.href_ $ renderRelativeRoute Route.Tachyons
          ]
        Lucid.title_ "Monadoc"
      Lucid.body_ [Lucid.class_ "bg-white black sans-serif"] $ do
        Lucid.h1_ [Lucid.class_ "purple sans-serif tc"] "Monadoc"
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

faviconHandler :: App.App request Wai.Response
faviconHandler = do
  config <- Reader.asks Context.config
  Common.fileResponse
    Http.ok200
    (Map.insert Http.hContentType "image/x-icon" $ Common.defaultHeaders config
    )
    "favicon.ico"

healthCheckHandler :: App.App request Wai.Response
healthCheckHandler = do
  pool <- Reader.asks Context.pool
  Trans.lift . Pool.withResource pool $ \connection -> do
    rows <- Sql.query_ connection "select 1"
    Monad.guard $ rows == [Sql.Only (1 :: Int)]
  config <- Reader.asks Context.config
  pure . Common.statusResponse Http.ok200 $ Common.defaultHeaders config

robotsHandler :: App.App request Wai.Response
robotsHandler = do
  config <- Reader.asks Context.config
  pure
    . Common.stringResponse Http.ok200 (Common.defaultHeaders config)
    $ unlines ["User-agent: *", "Disallow:"]

tachyonsHandler :: App.App request Wai.Response
tachyonsHandler = do
  config <- Reader.asks Context.config
  Common.fileResponse
    Http.ok200
    (Map.insert Http.hContentType "text/css;charset=utf-8"
    $ Common.defaultHeaders config
    )
    "tachyons-4-12-0.css"

throwHandler :: App.App request result
throwHandler = WithCallStack.throw $ userError "oh no"

notFoundHandler :: App.App request Wai.Response
notFoundHandler = do
  config <- Reader.asks Context.config
  pure . Common.statusResponse Http.notFound404 $ Common.defaultHeaders config
