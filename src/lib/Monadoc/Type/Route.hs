module Monadoc.Type.Route where

import Monadoc.Prelude
import qualified Monadoc.Type.Cabal.PackageName as PackageName
import qualified Monadoc.Type.Cabal.Version as Version
import qualified Monadoc.Type.Revision as Revision

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
  | Revision PackageName.PackageName Version.Version Revision.Revision
  | Robots
  | Search
  | Tachyons
  | Throw
  | Version PackageName.PackageName Version.Version
  deriving (Eq, Show)
