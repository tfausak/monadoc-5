module Monadoc.Type.Context
  ( Context(..)
  , fromConfig
  )
where

import qualified Control.Concurrent as Concurrent
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified GHC.Stack as Stack
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Vendor.Time as Time
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls

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

-- | Converts a config into a context. This involves acquiring any resources
-- described in the config.
fromConfig :: Stack.HasCallStack => Config.Config -> IO Context
fromConfig theConfig = do
  theManager <- Tls.newTlsManager
  let database = Config.database theConfig
  maxResources <- if isInMemory database then pure 1 else getMaxResources
  thePool <- Pool.createPool
    (Sql.open database)
    Sql.close
    stripCount
    idleTime
    maxResources
  pure Context
    { config = theConfig
    , manager = theManager
    , pool = thePool
    }

stripCount :: Int
stripCount = 1

idleTime :: Time.NominalDiffTime
idleTime = 60

getMaxResources :: IO Int
getMaxResources = do
  threadCount <- Concurrent.getNumCapabilities
  pure $ max 1 threadCount

isInMemory :: FilePath -> Bool
isInMemory database = case database of
  "" -> True
  ":memory:" -> True
  _ -> False
