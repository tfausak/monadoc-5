module Monadoc.Server.Router where

import qualified Data.Text as Text
import Monadoc.Prelude
import qualified Monadoc.Type.Cabal.PackageName as PackageName
import qualified Monadoc.Type.Cabal.Version as Version
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http

parseRoute :: Http.Method -> [Text.Text] -> Maybe Route.Route
parseRoute method path = do
  stdMethod <- either (always Nothing) Just <| Http.parseMethod method
  case (stdMethod, path) of
    (Http.GET, []) -> Just Route.Index
    (Http.GET, ["account"]) -> Just Route.Account
    (Http.GET, ["api", "github-callback"]) -> Just Route.GitHubCallback
    (Http.POST, ["api", "log-out"]) -> Just Route.LogOut
    (Http.GET, ["api", "ping"]) -> Just Route.Ping
    (Http.GET, ["api", "throw"]) -> Just Route.Throw
    (Http.GET, ["favicon.ico"]) -> Just Route.Favicon
    (Http.GET, ["package", p]) -> map Route.Package <| PackageName.fromText p
    (Http.GET, ["package", p, v]) -> do
      pkg <- PackageName.fromText p
      ver <- Version.fromText v
      pure <| Route.Version pkg ver
    (Http.GET, ["robots.txt"]) -> Just Route.Robots
    (Http.GET, ["search"]) -> Just Route.Search
    (Http.GET, ["static", "logo.png"]) -> Just Route.Logo
    (Http.GET, ["static", "tachyons-4-12-0.css"]) -> Just Route.Tachyons
    _ -> Nothing

renderRelativeRoute :: Route.Route -> Text.Text
renderRelativeRoute route = case route of
  Route.Account -> "/account"
  Route.Favicon -> "/favicon.ico"
  Route.GitHubCallback -> "/api/github-callback"
  Route.Index -> "/"
  Route.Logo -> "/static/logo.png"
  Route.LogOut -> "/api/log-out"
  Route.Package p -> "/package/" <> PackageName.toText p
  Route.Ping -> "/api/ping"
  Route.Robots -> "/robots.txt"
  Route.Search -> "/search"
  Route.Tachyons -> "/static/tachyons-4-12-0.css"
  Route.Throw -> "/api/throw"
  Route.Version p v -> "/package/" <> PackageName.toText p <> "/" <> Version.toText v

renderAbsoluteRoute :: Config.Config -> Route.Route -> Text.Text
renderAbsoluteRoute config =
  (Text.pack (Config.url config) <>) <<< renderRelativeRoute
