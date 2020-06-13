module Monadoc.Server.Router
  ( parseRoute
  , renderAbsoluteRoute
  , renderRelativeRoute
  )
where

import qualified Data.Text as Text
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http

parseRoute :: Http.Method -> [Text.Text] -> Maybe Route.Route
parseRoute method path = case (method, path) of
  ("GET", []) -> Just Route.Index
  ("GET", ["favicon.ico"]) -> Just Route.Favicon
  ("GET", ["health-check"]) -> Just Route.HealthCheck
  ("GET", ["robots.txt"]) -> Just Route.Robots
  ("GET", ["static", "logo.png"]) -> Just Route.Logo
  ("GET", ["static", "tachyons-4-12-0.css"]) -> Just Route.Tachyons
  ("GET", ["throw"]) -> Just Route.Throw
  _ -> Nothing

renderRelativeRoute :: Route.Route -> Text.Text
renderRelativeRoute route = case route of
  Route.Favicon -> "/favicon.ico"
  Route.HealthCheck -> "/health-check"
  Route.Index -> "/"
  Route.Logo -> "/static/logo.png"
  Route.Robots -> "/robots.txt"
  Route.Tachyons -> "/static/tachyons-4-12-0.css"
  Route.Throw -> "/throw"

renderAbsoluteRoute :: Config.Config -> Route.Route -> Text.Text
renderAbsoluteRoute config =
  mappend (Text.pack $ Config.url config) . renderRelativeRoute
