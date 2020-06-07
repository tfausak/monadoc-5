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
type App a = Reader.ReaderT Context.Context IO a

-- | Runs an 'App' action.
run :: Context.Context -> App a -> IO a
run = flip Reader.runReaderT

-- | Checks out a SQL connection from the pool and runs the given action with
-- it.
withConnection :: (Sql.Connection -> App a) -> App a
withConnection action = do
  pool <- Reader.asks Context.pool
  Pool.withResource pool action
