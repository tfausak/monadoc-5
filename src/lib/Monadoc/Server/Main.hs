module Monadoc.Server.Main
  ( run
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Pool as Pool
import qualified GHC.Clock as Clock
import qualified Monadoc.Console as Console
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Internal as Wai
import qualified System.Mem as Mem
import qualified Text.Printf as Printf

run :: App.App ()
run = do
  context <- Reader.ask
  Trans.lift
    . Warp.runSettings (Settings.fromConfig $ Context.config context)
    . middleware
    $ application context

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
    ("GET", ["health-check"]) -> healthCheckHandler
    ("GET", ["throw"]) -> throwHandler
    _ -> notFoundHandler

healthCheckHandler :: Handler.Handler Wai.Response
healthCheckHandler = do
  pool <- Reader.asks $ Context.pool . fst
  Trans.lift . Pool.withResource pool $ \connection -> do
    rows <- Sql.query_ connection "select 1"
    Monad.guard $ rows == [Sql.Only (1 :: Int)]
  pure $ statusResponse Http.ok200 []

throwHandler :: Handler.Handler a
throwHandler = WithCallStack.throw $ userError "oh no"

notFoundHandler :: Handler.Handler Wai.Response
notFoundHandler = pure $ statusResponse Http.notFound404 []

middleware :: Wai.Middleware
middleware = logRequests . addContentLength . handleExceptions

logRequests :: Wai.Middleware
logRequests handle request respond = do
  timeBefore <- Clock.getMonotonicTime
  allocationsBefore <- Mem.getAllocationCounter
  handle request $ \response -> do
    allocationsAfter <- Mem.getAllocationCounter
    timeAfter <- Clock.getMonotonicTime
    Console.info $ Printf.printf
      "%d %s %s%s %.3f %d"
      (Http.statusCode $ Wai.responseStatus response)
      (Utf8.toString $ Wai.requestMethod request)
      (Utf8.toString $ Wai.rawPathInfo request)
      (Utf8.toString $ Wai.rawQueryString request)
      (timeAfter - timeBefore)
      (div (allocationsBefore - allocationsAfter) 1024)
    respond response

addContentLength :: Wai.Middleware
addContentLength handle request respond = handle request $ \oldResponse ->
  respond $ case oldResponse of
    Wai.ResponseBuilder status headers builder ->
      let
        size = LazyByteString.length $ Builder.toLazyByteString builder
        header = (Http.hContentLength, Utf8.fromString $ show size)
      in Wai.ResponseBuilder status (header : headers) builder
    _ -> oldResponse

handleExceptions :: Wai.Middleware
handleExceptions handle request respond =
  Exception.catch (handle request respond) $ \someException -> do
    Settings.onException (Just request) someException
    respond $ Settings.onExceptionResponse someException

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers = stringResponse status headers $ unwords
  [show $ Http.statusCode status, Utf8.toString $ Http.statusMessage status]

stringResponse :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
stringResponse status headers string = Wai.responseLBS
  status
  ((Http.hContentType, "text/plain; charset=utf-8") : headers)
  (LazyByteString.fromStrict $ Utf8.fromString string)
