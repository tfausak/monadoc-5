module Monadoc.Server.Application
  ( application
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Pool as Pool
import qualified Lucid
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Package
import qualified System.FilePath as FilePath

application :: Context.Context request -> Wai.Application
application context request respond = do
  let ctx = context { Context.request = request }
  response <- App.run ctx . runRoute $ getRoute request
  respond response

getRoute :: Wai.Request -> Maybe Route.Route
getRoute request =
  let
    method = Wai.requestMethod request
    path = Wai.pathInfo request
  in case (method, path) of
    ("GET", []) -> Just Route.Index
    ("GET", ["favicon.ico"]) -> Just Route.Favicon
    ("GET", ["health-check"]) -> Just Route.HealthCheck
    ("GET", ["robots.txt"]) -> Just Route.Robots
    ("GET", ["tachyons.css"]) -> Just Route.Tachyons
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

rootHandler :: App.App request Wai.Response
rootHandler = do
  config <- Reader.asks Context.config
  pure
    . Common.responseBS
        Http.ok200
        (Map.insert Http.hContentType "text/html;charset=utf-8"
        $ Common.defaultHeaders config
        )
    . LazyByteString.toStrict
    . Lucid.renderBS
    $ do
        Lucid.doctype_
        Lucid.html_ [Lucid.lang_ "en-US"] $ do
          Lucid.head_ $ do
            Lucid.meta_ [Lucid.charset_ "utf-8"]
            Lucid.meta_
              [ Lucid.name_ "description"
              , Lucid.content_ "Better Haskell documentation."
              ]
            Lucid.meta_ [Lucid.name_ "og:title", Lucid.content_ "Monadoc"]
            Lucid.meta_ [Lucid.name_ "og:type", Lucid.content_ "website"]
            Lucid.link_ [Lucid.rel_ "icon", Lucid.href_ "favicon.ico"]
            Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "tachyons.css"]
            Lucid.title_ "Monadoc"
          Lucid.body_ $ do
            Lucid.h1_ [Lucid.class_ "purple sans-serif tc"] "Monadoc"

faviconHandler :: App.App request Wai.Response
faviconHandler = do
  config <- Reader.asks Context.config
  fileResponse
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
  fileResponse
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

fileResponse
  :: Http.Status -> Common.Headers -> FilePath -> App.App request Wai.Response
fileResponse status headers name = Trans.lift $ do
  let relative = FilePath.combine "data" name
  absolute <- Package.getDataFileName relative
  contents <- ByteString.readFile absolute
  pure $ Common.responseBS status headers contents
