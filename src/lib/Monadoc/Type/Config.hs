module Monadoc.Type.Config
  ( Config(..)
  , initial
  )
where

import qualified Network.Wai.Handler.Warp as Warp

-- | Application configuration. This contains all the stuff necessary to start
-- things up. Descriptions and defaults are available by running the executable
-- with @--help@.
--
-- It is expected that each field here will have a corresponding option defined
-- in "Monadoc.Data.Options".
data Config = Config
  { clientId :: String
  -- ^ The client ID for the GitHub OAuth application. Make sure this goes with
  -- the secret.
  , clientSecret :: String
  -- ^ The client secret for the GitHub OAuth application. Make sure this goes
  -- with the ID.
  , database :: FilePath
  -- ^ The path to the database file. To use an in-memory databse, set this to
  -- either the empty string or @":memory:"@.
  , help :: Bool
  -- ^ Whether or not the help should be shown.
  , host :: Warp.HostPreference
  -- ^ The host to bind on. In typical usage you'll want to set this to @"*"@.
  , port :: Warp.Port
  -- ^ The port to bind on.
  , url :: String
  -- ^ The base URL that the site is available at. Be sure to change this if
  -- you change the port.
  , version :: Bool
  -- ^ Whether or not to show the version number.
  } deriving (Eq, Show)

-- | The default config. These values are optimized for development.
initial :: Config
initial = Config
  { clientId = "235ce8c873f4ed90905c"
  , clientSecret = "48e202a2b3aa30ad2a4e844f77b7d10807ab1deb"
  , database = "monadoc.sqlite3"
  , help = False
  , host = "127.0.0.1"
  , port = 4444
  , url = "http://localhost:4444"
  , version = False
  }
