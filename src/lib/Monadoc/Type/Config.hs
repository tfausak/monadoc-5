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
  -- the secret. The default will work for local development, but you'll need
  -- to make your own app to get this working in any other environments.
  , clientSecret :: String
  -- ^ The client secret for the GitHub OAuth application. Make sure this goes
  -- with the ID.
  , database :: FilePath
  -- ^ The path to the database file. To use an in-memory databse, set this to
  -- either the empty string or @":memory:"@.
  , discordUrl :: String
  -- ^ A URL to execute a Discord webhook. This is currently used for easy
  -- exception reporting. If you don't have a Discord server set up, you can
  -- ignore this. It's not critical.
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
  , discordUrl = "http://invalid/api/webhooks/id/token"
  , help = False
  , host = "127.0.0.1"
  , port = 4444
  , url = "http://localhost:4444"
  , version = False
  }
