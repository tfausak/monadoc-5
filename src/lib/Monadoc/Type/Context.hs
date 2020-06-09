module Monadoc.Type.Context
  ( Context(..)
  )
where

import qualified Data.Pool as Pool
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Client as Client

-- | The run-time application context. This will be available in most places
-- and is basically a grab bag of global state.
data Context = Context
  { config :: Config.Config
  -- ^ The config used to create this context, just in case we still need some
  -- values from it.
  , manager :: Client.Manager
  -- ^ A manager for making HTTP requests.
  , pool :: Pool.Pool Sql.Connection
  -- ^ A pool of SQLite connections for talking to the database.
  }
