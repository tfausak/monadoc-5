module Monadoc.Server.Application where

import qualified Monadoc.Handler.Account as Handler.Account
import qualified Monadoc.Handler.Favicon as Handler.Favicon
import qualified Monadoc.Handler.GitHubCallback as Handler.GitHubCallback
import qualified Monadoc.Handler.Index as Handler.Index
import qualified Monadoc.Handler.Logo as Handler.Logo
import qualified Monadoc.Handler.LogOut as Handler.LogOut
import qualified Monadoc.Handler.Ping as Handler.Ping
import qualified Monadoc.Handler.Robots as Handler.Robots
import qualified Monadoc.Handler.Search as Handler.Search
import qualified Monadoc.Handler.Tachyons as Handler.Tachyons
import qualified Monadoc.Handler.Throw as Handler.Throw
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.NotFoundException as NotFoundException
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Network.Wai as Wai
import Monadoc.Prelude

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
runRoute maybeRoute = do
  route <- maybe
    (WithCallStack.throw NotFoundException.NotFoundException)
    pure
    maybeRoute
  case route of
    Route.Account -> Handler.Account.handle
    Route.Favicon -> Handler.Favicon.handle
    Route.GitHubCallback -> Handler.GitHubCallback.handle
    Route.Index -> Handler.Index.handle
    Route.Logo -> Handler.Logo.handle
    Route.LogOut -> Handler.LogOut.handle
    Route.Ping -> Handler.Ping.handle
    Route.Robots -> Handler.Robots.handle
    Route.Search -> Handler.Search.handle
    Route.Tachyons -> Handler.Tachyons.handle
    Route.Throw -> Handler.Throw.handle
