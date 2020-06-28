module Monadoc.Type.App
  ( App
  , run
  , sql
  , sql_
  , withConnection
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Monadoc.Console as Console
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.Sql as Sql

-- | The main application type. This simply provides the run-time context. Use
-- 'run' to convert this into 'IO'.
type App request result = Reader.ReaderT (Context.Context request) IO result

-- | Runs a SQL query with logging and returns the results.
sql :: (Sql.FromRow b, Sql.ToRow a) => Sql.Query -> a -> App request [b]
sql query params = do
  Console.info $ "[sql] " <> Text.unpack (Sql.fromQuery query)
  withConnection
    $ \connection -> Trans.lift $ Sql.query connection query params

-- | Runs a SQL query with logging and discards the results.
sql_ :: Sql.ToRow a => Sql.Query -> a -> App request ()
sql_ query params =
  Monad.void (sql query params :: App request [[Sql.SQLData]])

-- | Runs an 'App' action.
run :: Context.Context request -> App request result -> IO result
run = flip Reader.runReaderT

-- | Checks out a SQL connection from the pool and runs the given action with
-- it.
withConnection :: (Sql.Connection -> App request result) -> App request result
withConnection action = do
  pool <- Reader.asks Context.pool
  Pool.withResource pool action
