module Monadoc.Type.Route where

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
  | Ping
  | Robots
  | Search
  | Tachyons
  | Throw
  deriving (Eq, Show)
