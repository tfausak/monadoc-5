module Monadoc.Type.Context where

import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Config as Config
import qualified Network.HTTP.Client as Client
import qualified Test.Hspec as Hspec

-- | The run-time application context. This will be available in most places
-- and is basically a grab bag of global state.
data Context request = Context
  { config :: Config.Config
  -- ^ The config used to create this context, just in case we still need some
  -- values from it.
  , manager :: Client.Manager
  -- ^ A manager for making HTTP requests.
  , pool :: Pool.Pool Sql.Connection
  -- ^ A pool of SQLite connections for talking to the database.
  , request :: request
  -- ^ An optional request. Since the app runs as both a server and a worker,
  -- the presence of a request is communicated through the types rather than
  -- using something like @Maybe Request@.
  }

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Context" $ do

  Hspec.it "needs tests" Hspec.pending
