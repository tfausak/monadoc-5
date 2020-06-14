module Monadoc.Main
  ( run
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified GHC.Stack as Stack
import qualified Monadoc.Console as Console
import qualified Monadoc.Data.Migrations as Migrations
import qualified Monadoc.Server.Main as Server
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Type.Service as Service
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Vendor.Time as Time
import qualified Monadoc.Worker.Main as Worker

run :: Stack.HasCallStack => App.App request ()
run = do
  runMigrations
  context <- Reader.ask
  Trans.lift
    . Async.mapConcurrently_ (App.run context . runService)
    . Config.services
    $ Context.config context

runService :: Stack.HasCallStack => Service.Service -> App.App request ()
runService service = case service of
  Service.Server -> Server.run
  Service.Worker -> Worker.run

runMigrations :: App.App request ()
runMigrations = do
  Console.info "Running migrations ..."
  App.withConnection $ \connection -> Trans.lift $ do
    Sql.execute_ connection "pragma journal_mode = wal"
    Sql.execute_
      connection
      "create table if not exists migrations \
      \( iso8601 text not null primary key \
      \, sha256 text not null )"
  mapM_ ensureMigration Migrations.migrations

ensureMigration :: Migration.Migration -> App.App request ()
ensureMigration migration = App.withConnection $ \connection ->
  Trans.lift . Sql.withTransaction connection $ do
    maybeDigest <- getDigest connection $ Migration.timestamp migration
    case maybeDigest of
      Nothing -> runMigration connection migration
      Just digest -> checkDigest migration digest

getDigest :: Sql.Connection -> Timestamp.Timestamp -> IO (Maybe Sha256.Sha256)
getDigest connection timestamp = do
  rows <- Sql.query
    connection
    "select sha256 from migrations where iso8601 = ?"
    [timestamp]
  pure $ case rows of
    [] -> Nothing
    Sql.Only sha256 : _ -> Just sha256

runMigration :: Sql.Connection -> Migration.Migration -> IO ()
runMigration connection migration = do
  Console.info $ unwords
    [ "Running migration"
    , Time.formatTime "%Y-%m-%dT%H:%M:%S%3QZ"
    . Timestamp.toUtcTime
    $ Migration.timestamp migration
    , "..."
    ]
  Sql.execute_ connection $ Migration.query migration
  Sql.execute
    connection
    "insert into migrations (iso8601, sha256) values (?, ?)"
    migration

checkDigest
  :: Exception.MonadThrow m => Migration.Migration -> Sha256.Sha256 -> m ()
checkDigest migration expectedSha256 = do
  let actualSha256 = Migration.sha256 migration
  Monad.when (actualSha256 /= expectedSha256) $ Exception.throwM
    MigrationMismatch.MigrationMismatch
      { MigrationMismatch.actual = actualSha256
      , MigrationMismatch.expected = expectedSha256
      , MigrationMismatch.timestamp = Migration.timestamp migration
      }
