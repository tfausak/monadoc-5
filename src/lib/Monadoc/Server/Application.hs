module Monadoc.Server.Application
  ( application
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Handler.Favicon as Handler.Favicon
import qualified Monadoc.Handler.HealthCheck as Handler.HealthCheck
import qualified Monadoc.Handler.Index as Handler.Index
import qualified Monadoc.Handler.Logo as Handler.Logo
import qualified Monadoc.Handler.Robots as Handler.Robots
import qualified Monadoc.Handler.Tachyons as Handler.Tachyons
import qualified Monadoc.Handler.Throw as Handler.Throw
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
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
  Router.parseRoute (Wai.requestMethod request) (Wai.pathInfo request)

runRoute :: Maybe Route.Route -> App.App Wai.Request Wai.Response
runRoute maybeRoute = case maybeRoute of
  Just route -> case route of
    Route.Favicon -> Handler.Favicon.handle
    Route.HealthCheck -> Handler.HealthCheck.handle
    Route.Index -> Handler.Index.handle
    Route.Logo -> Handler.Logo.handle
    Route.Robots -> Handler.Robots.handle
    Route.Tachyons -> Handler.Tachyons.handle
    Route.Throw -> Handler.Throw.handle
  Nothing -> do
    config <- Reader.asks Context.config
    pure . Common.statusResponse Http.notFound404 $ Common.defaultHeaders
      config
