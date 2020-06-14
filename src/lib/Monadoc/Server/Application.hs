module Monadoc.Server.Application
  ( application
  )
where

import qualified GHC.Stack as Stack
import qualified Monadoc.Handler.Favicon as Handler.Favicon
import qualified Monadoc.Handler.Index as Handler.Index
import qualified Monadoc.Handler.Logo as Handler.Logo
import qualified Monadoc.Handler.Ping as Handler.Ping
import qualified Monadoc.Handler.Robots as Handler.Robots
import qualified Monadoc.Handler.Tachyons as Handler.Tachyons
import qualified Monadoc.Handler.Throw as Handler.Throw
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.NotFoundException as NotFoundException
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Network.Wai as Wai

application :: Stack.HasCallStack => Context.Context request -> Wai.Application
application context request respond = do
  response <-
    App.run context { Context.request = request } . runRoute $ parseRoute
      request
  respond response

parseRoute :: Wai.Request -> Maybe Route.Route
parseRoute request =
  Router.parseRoute (Wai.requestMethod request) (Wai.pathInfo request)

runRoute
  :: Stack.HasCallStack
  => Maybe Route.Route
  -> App.App Wai.Request Wai.Response
runRoute maybeRoute = do
  route <- maybe
    (WithCallStack.throw NotFoundException.NotFoundException)
    pure
    maybeRoute
  case route of
    Route.Favicon -> Handler.Favicon.handle
    Route.Index -> Handler.Index.handle
    Route.Logo -> Handler.Logo.handle
    Route.Ping -> Handler.Ping.handle
    Route.Robots -> Handler.Robots.handle
    Route.Tachyons -> Handler.Tachyons.handle
    Route.Throw -> Handler.Throw.handle
