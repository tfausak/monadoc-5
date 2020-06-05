module Monadoc.Type.Config
  ( Config(..)
  , initial
  )
where

import qualified Data.String as String
import qualified Network.Wai.Handler.Warp as Warp

-- | Application configuration. This contains all the stuff necessary to start
-- things up. Descriptions and defaults are available by running the executable
-- with @--help@.
data Config = Config
  { database :: FilePath
  -- ^ The path to the database file. To use an in-memory databse, set this to
  -- either the empty string or @":memory:"@.
  , help :: Bool
  -- ^ Whether or not the help should be shown.
  , host :: Warp.HostPreference
  -- ^ The host to bind on. In typical usage you'll want to set this to @"*"@.
  , port :: Warp.Port
  -- ^ The port to bind on.
  , version :: Bool
  -- ^ Whether or not to show the version number.
  } deriving (Eq, Show)

-- | The default config. These values are optimized for development.
initial :: Config
initial = Config
  { database = "monadoc.sqlite3"
  , help = False
  , host = String.fromString "127.0.0.1"
  , port = 4444
  , version = False
  }
