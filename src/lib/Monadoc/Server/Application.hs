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
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Package
import qualified System.FilePath as FilePath

application :: Context.Context -> Wai.Application
application context request respond = do
  response <- App.run context . runHandler request $ route request
  respond response

runHandler :: Wai.Request -> Handler.Handler a -> App.App a
runHandler request handler = do
  context <- Reader.ask
  Trans.lift $ Handler.run context request handler

route :: Wai.Request -> Handler.Handler Wai.Response
route request =
  let
    method = Wai.requestMethod request
    path = Wai.pathInfo request
  in case (method, path) of
    ("GET", []) -> rootHandler
    ("GET", ["favicon.ico"]) -> faviconHandler
    ("GET", ["health-check"]) -> healthCheckHandler
    ("GET", ["robots.txt"]) -> robotsHandler
    ("GET", ["tachyons.css"]) -> tachyonsHandler
    ("GET", ["throw"]) -> throwHandler
    _ -> notFoundHandler

rootHandler :: Handler.Handler Wai.Response
rootHandler =
  pure
    . Settings.responseBS
        Http.ok200
        (Map.singleton Http.hContentType "text/html;charset=utf-8")
    . LazyByteString.toStrict
    . Lucid.renderBS
    $ do
        Lucid.doctype_
        Lucid.html_ [Lucid.lang_ "en-US"] $ do
          Lucid.head_ $ do
            Lucid.meta_ [Lucid.charset_ "utf-8"]
            Lucid.link_ [Lucid.rel_ "icon", Lucid.href_ "favicon.ico"]
            Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "tachyons.css"]
            Lucid.title_ "Monadoc"
          Lucid.body_ $ do
            Lucid.h1_ [Lucid.class_ "purple sans-serif tc"] "Monadoc"

faviconHandler :: Handler.Handler Wai.Response
faviconHandler = fileResponse
  Http.ok200
  (Map.singleton Http.hContentType "image/x-icon")
  "favicon.ico"

healthCheckHandler :: Handler.Handler Wai.Response
healthCheckHandler = do
  pool <- Reader.asks $ Context.pool . fst
  Trans.lift . Pool.withResource pool $ \connection -> do
    rows <- Sql.query_ connection "select 1"
    Monad.guard $ rows == [Sql.Only (1 :: Int)]
  pure $ Settings.statusResponse Http.ok200 Map.empty

robotsHandler :: Handler.Handler Wai.Response
robotsHandler = pure . Settings.stringResponse Http.ok200 Map.empty $ unlines
  ["User-agent: *", "Disallow:"]

tachyonsHandler :: Handler.Handler Wai.Response
tachyonsHandler = fileResponse
  Http.ok200
  (Map.singleton Http.hContentType "text/css;charset=utf-8")
  "tachyons-4-12-0.css"

throwHandler :: Handler.Handler a
throwHandler = WithCallStack.throw $ userError "oh no"

notFoundHandler :: Handler.Handler Wai.Response
notFoundHandler = pure $ Settings.statusResponse Http.notFound404 Map.empty

fileResponse
  :: Http.Status
  -> Settings.Headers
  -> FilePath
  -> Handler.Handler Wai.Response
fileResponse status headers name = Trans.lift $ do
  let relative = FilePath.combine "data" name
  absolute <- Package.getDataFileName relative
  contents <- ByteString.readFile absolute
  pure $ Settings.responseBS status headers contents
