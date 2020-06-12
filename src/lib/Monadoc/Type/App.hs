module Monadoc.Type.App
  ( App
  , run
  , withConnection
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Pool as Pool
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.Sql as Sql

-- | The main application type. This simply provides the run-time context. Use
-- 'run' to convert this into 'IO'.
type App request result = Reader.ReaderT (Context.Context request) IO result

-- | Runs an 'App' action.
run :: Context.Context request -> App request result -> IO result
run = flip Reader.runReaderT

-- | Checks out a SQL connection from the pool and runs the given action with
-- it.
withConnection :: (Sql.Connection -> App request result) -> App request result
withConnection action = do
  pool <- Reader.asks Context.pool
  Pool.withResource pool action
