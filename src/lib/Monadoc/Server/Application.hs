module Monadoc.Server.Application
  ( application
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Pool as Pool
import qualified Lucid
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Utility.Utf8 as Utf8
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
    . Wai.responseLBS
        Http.ok200
        [(Http.hContentType, "text/html; charset=utf-8")]
    . Lucid.renderBS
    . Lucid.doctypehtml_
    $ do
        Lucid.head_ $ do
          Lucid.meta_ [Lucid.charset_ "utf-8"]
          Lucid.link_ [Lucid.rel_ "icon", Lucid.href_ "favicon.ico"]
          Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "tachyons.css"]
          Lucid.title_ "Monadoc"
        Lucid.body_ $ do
          Lucid.h1_ [Lucid.class_ "purple sans-serif tc"] "Monadoc"

faviconHandler :: Handler.Handler Wai.Response
faviconHandler = Trans.lift $ do
  let relative = FilePath.combine "data" "favicon.ico"
  absolute <- Package.getDataFileName relative
  contents <- LazyByteString.readFile absolute
  pure $ Wai.responseLBS
    Http.ok200
    [(Http.hContentType, "image/vnd.microsoft.icon")]
    contents

healthCheckHandler :: Handler.Handler Wai.Response
healthCheckHandler = do
  pool <- Reader.asks $ Context.pool . fst
  Trans.lift . Pool.withResource pool $ \connection -> do
    rows <- Sql.query_ connection "select 1"
    Monad.guard $ rows == [Sql.Only (1 :: Int)]
  pure $ statusResponse Http.ok200 []

robotsHandler :: Handler.Handler Wai.Response
robotsHandler =
  pure . stringResponse Http.ok200 [] $ unlines ["User-agent: *", "Disallow:"]

tachyonsHandler :: Handler.Handler Wai.Response
tachyonsHandler = Trans.lift $ do
  let relative = FilePath.combine "data" "tachyons-4-12-0.css"
  absolute <- Package.getDataFileName relative
  contents <- LazyByteString.readFile absolute
  pure $ Wai.responseLBS
    Http.ok200
    [(Http.hContentType, "text/css; charset=utf-8")]
    contents

throwHandler :: Handler.Handler a
throwHandler = WithCallStack.throw $ userError "oh no"

notFoundHandler :: Handler.Handler Wai.Response
notFoundHandler = pure $ statusResponse Http.notFound404 []

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers = stringResponse status headers $ unwords
  [show $ Http.statusCode status, Utf8.toString $ Http.statusMessage status]

stringResponse :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
stringResponse status headers string = Wai.responseLBS
  status
  ((Http.hContentType, "text/plain; charset=utf-8") : headers)
  (LazyByteString.fromStrict $ Utf8.fromString string)
