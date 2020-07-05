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
  ("GET", ["account"]) -> Just Route.Account
  ("GET", ["api", "github-callback"]) -> Just Route.GitHubCallback
  ("POST", ["api", "log-out"]) -> Just Route.LogOut
  ("GET", ["api", "ping"]) -> Just Route.Ping
  ("GET", ["api", "throw"]) -> Just Route.Throw
  ("GET", ["favicon.ico"]) -> Just Route.Favicon
  ("GET", ["robots.txt"]) -> Just Route.Robots
  ("GET", ["search"]) -> Just Route.Search
  ("GET", ["static", "logo.png"]) -> Just Route.Logo
  ("GET", ["static", "tachyons-4-12-0.css"]) -> Just Route.Tachyons
  _ -> Nothing

renderRelativeRoute :: Route.Route -> Text.Text
renderRelativeRoute route = case route of
  Route.Account -> "/account"
  Route.Favicon -> "/favicon.ico"
  Route.GitHubCallback -> "/api/github-callback"
  Route.Index -> "/"
  Route.Logo -> "/static/logo.png"
  Route.LogOut -> "/api/log-out"
  Route.Ping -> "/api/ping"
  Route.Robots -> "/robots.txt"
  Route.Search -> "/search"
  Route.Tachyons -> "/static/tachyons-4-12-0.css"
  Route.Throw -> "/api/throw"

renderAbsoluteRoute :: Config.Config -> Route.Route -> Text.Text
renderAbsoluteRoute config =
  mappend (Text.pack $ Config.url config) . renderRelativeRoute
