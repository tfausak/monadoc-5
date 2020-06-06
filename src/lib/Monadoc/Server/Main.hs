module Monadoc.Server.Main
  ( run
  )
where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Internal as Wai

run :: Stack.HasCallStack => App.App ()
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

runHandler :: Wai.Request -> Handler.Handler -> App.App Wai.Response
runHandler request handler = do
  context <- Reader.ask
  Trans.lift $ Handler.run context request handler

route :: Wai.Request -> Handler.Handler
route request =
  let
    method = Utf8.toString $ Wai.requestMethod request
    path = Text.unpack <$> Wai.pathInfo request
  in case (method, path) of
    ("GET", ["health-check"]) -> healthCheckHandler
    ("GET", ["throw"]) -> throwHandler
    _ -> notFoundHandler

healthCheckHandler :: Handler.Handler
healthCheckHandler = pure $ statusResponse Http.ok200 []

throwHandler :: Handler.Handler
throwHandler = WithCallStack.throw $ userError "oh no"

notFoundHandler :: Handler.Handler
notFoundHandler = pure $ statusResponse Http.notFound404 []

middleware :: Wai.Middleware
middleware = addContentLength . handleExceptions

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
handleExceptions handle request respond = Exception.catch
  (handle request respond)
  $ \ someException -> do
    Settings.onException (Just request) someException
    respond $ Settings.onExceptionResponse someException

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers = stringResponse status headers $ unwords
  [show $ Http.statusCode status, Utf8.toString $ Http.statusMessage status]

stringResponse :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
stringResponse status headers string = Wai.responseLBS
  status
  ((Http.hContentType, Utf8.fromString "text/plain; charset=utf-8") : headers)
  (LazyByteString.fromStrict $ Utf8.fromString string)
