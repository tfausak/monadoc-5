module Monadoc.Type.Route
  ( Route(..)
  )
where

-- | All of the routes that are reachable from the server. Unless otherwise
-- noted, routes probably respond to HTTP GET requests. It is expected that
-- each route @R@ has a corresponding handler at @Monadoc.Handler.R@.
data Route
  = Favicon
  | HealthCheck
  | Index
  | Logo
  | Robots
  | Tachyons
  | Throw
  deriving (Eq, Show)
