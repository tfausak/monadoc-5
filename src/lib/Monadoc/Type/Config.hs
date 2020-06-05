module Monadoc.Type.Config
  ( Config(..)
  , initial
  )
where

import qualified Data.String as String
import qualified Network.Wai.Handler.Warp as Warp

data Config = Config
  { database :: String
  , help :: Bool
  , host :: Warp.HostPreference
  , port :: Warp.Port
  , version :: Bool
  } deriving (Eq, Show)

initial :: Config
initial = Config
  { database = "monadoc.sqlite3"
  , help = False
  , host = String.fromString "127.0.0.1"
  , port = 4444
  , version = False
  }
