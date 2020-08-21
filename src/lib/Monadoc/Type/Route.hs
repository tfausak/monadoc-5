module Monadoc.Type.Route where

import qualified Test.Hspec as Hspec

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

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Route" $ do

  Hspec.it "needs tests" Hspec.pending
