module Monadoc
  ( monadoc
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified GHC.Stack as Stack
import qualified Monadoc.Console as Console
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Migrations as Migrations
import qualified Monadoc.Data.Options as Options
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Server.Main as Server
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Vendor.Sqlite as Sql
import qualified Monadoc.Worker.Main as Worker
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

monadoc :: Stack.HasCallStack => IO ()
monadoc = do
  config <- getConfig
  Console.info $ unwords
    ["Starting Monadoc version"
    , Version.string
    , "commit"
    , Maybe.fromMaybe "unknown" Commit.hash
    , "..."
    ]
  context <- Context.fromConfig config
  App.run context migrate
  Async.race_ (App.run context Server.run) (App.run context Worker.run)

migrate :: Stack.HasCallStack => App.App ()
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
        Monad.when (actualSha256 /= expectedSha256)
        $ Exception.throwM MigrationMismatch.MigrationMismatch
          { MigrationMismatch.actual = actualSha256
          , MigrationMismatch.expected = expectedSha256
          , MigrationMismatch.timestamp = timestamp
          }

withConnection :: Stack.HasCallStack => (Sql.Connection -> App.App a) -> App.App a
withConnection app = do
  pool <- Reader.asks Context.pool
  Pool.withResource pool app

getConfig :: Stack.HasCallStack => IO Config.Config
getConfig = do
  arguments <- Environment.getArgs
  let (funs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute Options.options arguments
  Monad.forM_ args $ \arg ->
    IO.hPutStrLn IO.stderr $ "WARNING: argument `" <> arg <> "' not expected"
  Monad.forM_ opts $ \opt ->
    IO.hPutStrLn IO.stderr $ "WARNING: option `" <> opt <> "' not recognized"
  Monad.forM_ errs $ \err -> IO.hPutStr IO.stderr $ "ERROR: " <> err
  Monad.unless (null errs) Exit.exitFailure
  config <- case Monad.foldM (flip ($)) Config.initial funs of
    Left err -> do
      IO.hPutStrLn IO.stderr $ "ERROR: " <> err
      Exit.exitFailure
    Right cfg -> pure cfg
  Monad.when (Config.help config) $ do
    name <- Environment.getProgName
    let extra = case Commit.hash of
          Nothing -> []
          Just hash -> ["commit", hash]
    putStr $ GetOpt.usageInfo (unwords $ [name, "version", Version.string] <> extra) Options.options
    Exit.exitSuccess
  Monad.when (Config.version config) $ do
    putStrLn $ Version.string <> case Commit.hash of
      Nothing -> ""
      Just hash -> "-" <> hash
    Exit.exitSuccess
  pure config
