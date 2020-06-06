module Monadoc.Main
  ( run
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Pool as Pool
import qualified Monadoc.Data.Migrations as Migrations
import qualified Monadoc.Server.Main as Server
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Worker.Main as Worker

run :: App.App ()
run = do
  migrate
  context <- Reader.ask
  Trans.lift
    $ Async.race_ (App.run context Server.run) (App.run context Worker.run)

migrate :: App.App ()
migrate = withConnection $ \connection -> Trans.lift $ do
  Sql.execute_ connection $ Sql.sql "pragma journal_mode = wal"
  Sql.execute_ connection
    $ Sql.sql
        "create table if not exists migrations \
        \( iso8601 text not null primary key \
        \, sha256 text not null )"
  digests <- fmap Map.fromList . Sql.query_ connection $ Sql.sql
    "select iso8601, sha256 from migrations"
  Monad.forM_ Migrations.migrations $ \migration -> do
    let timestamp = Migration.timestamp migration
    let actualSha256 = Migration.sha256 migration
    case Map.lookup timestamp digests of
      Nothing -> Sql.withTransaction connection $ do
        Sql.execute_ connection $ Migration.query migration
        Sql.execute
          connection
          (Sql.sql "insert into migrations (iso8601, sha256) values (?, ?)")
          migration
      Just expectedSha256 ->
        Monad.when (actualSha256 /= expectedSha256) $ Exception.throwM
          MigrationMismatch.MigrationMismatch
            { MigrationMismatch.actual = actualSha256
            , MigrationMismatch.expected = expectedSha256
            , MigrationMismatch.timestamp = timestamp
            }

withConnection :: (Sql.Connection -> App.App a) -> App.App a
withConnection app = do
  pool <- Reader.asks Context.pool
  Pool.withResource pool app
