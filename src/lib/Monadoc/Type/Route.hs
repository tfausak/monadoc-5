module Monadoc.Type.Route where

import Monadoc.Prelude
import qualified Monadoc.Type.Cabal.PackageName as PackageName

-- | All of the routes that are reachable from the server. Unless otherwise
-- noted, routes probably respond to HTTP GET requests. It is expected that
-- each route @R@ has a corresponding handler at @Monadoc.Handler.R@.
data Route
  = Account
  | Favicon
  | GitHubCallback
  | Index
  | Logo
  | LogOut -- ^ POST
  | Package PackageName.PackageName
  | Ping
  | Robots
  | Search
  | Tachyons
  | Throw
  deriving (Eq, Show)
