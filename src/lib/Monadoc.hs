module Monadoc ( monadoc ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Fixed as Fixed
import qualified Data.Map as Map
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

monadoc :: IO ()
monadoc = do
  config <- getConfig
  context <- makeContext config
  runApp context migrate
  Async.race_ (runApp context server) (runApp context worker)

migrate :: App ()
migrate = withConnection $ \ connection -> Trans.lift $ do
  Sql.execute_ connection $ query "pragma journal_mode = wal"
  Sql.execute_ connection $ query
    "create table if not exists migrations \
    \( iso8601 text not null primary key \
    \, sha256 text not null )"
  digests <- fmap Map.fromList . Sql.query_ connection $ query
    "select iso8601, sha256 from migrations"
  Monad.forM_ migrations $ \ migration -> do
    let iso8601 = migrationIso8601 migration
    let actualSha256 = migrationSha256 migration
    case Map.lookup iso8601 digests of
      Nothing -> Sql.withTransaction connection $ do
        Sql.execute_ connection $ migrationQuery migration
        Sql.execute connection
          (query "insert into migrations (iso8601, sha256) values (?, ?)")
          (iso8601, actualSha256)
      Just expectedSha256 -> Monad.when (actualSha256 /= expectedSha256)
        . Exception.throwM
        $ MigrationMismatch iso8601 expectedSha256 actualSha256

data MigrationMismatch = MigrationMismatch
  { migrationMismatchIso8601 :: Iso8601
  , migrationMismatchExpectedSha256 :: Sha256
  , migrationMismatchActualSha256 :: Sha256
  } deriving (Eq, Show)

instance Exception.Exception MigrationMismatch

data Migration = Migration
  { migrationIso8601 :: Iso8601
  , migrationQuery :: Sql.Query
  } deriving (Eq, Show)

migrationSha256 :: Migration -> Sha256
migrationSha256 =
  Sha256 . Crypto.hash . Text.encodeUtf8 . Sql.fromQuery . migrationQuery

migrations :: [Migration]
migrations =
  [ Migration
    { migrationIso8601 = makeIso8601 2020 5 31 13 38 0
    , migrationQuery = query "select 1"
    }
  ]

makeIso8601 :: Integer -> Int -> Int -> Int -> Int -> Fixed.Pico -> Iso8601
makeIso8601 year month day hour minute second = Iso8601 Time.UTCTime
  { Time.utctDay = Time.fromGregorian year month day
  , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
    { Time.todHour = hour
    , Time.todMin = minute
    , Time.todSec = second
    }
  }

newtype Sha256 = Sha256
  { unwrapSha256 :: Crypto.Digest Crypto.SHA256
  } deriving (Eq, Ord, Show)

instance Sql.FromField Sha256 where
  fromField field = do
    string <- Sql.fromField field
    case Read.readMaybe string of
      Nothing -> Sql.returnError Sql.ConversionFailed field
        $ "failed to parse: " <> show string
      Just digest -> pure $ Sha256 digest

instance Sql.ToField Sha256 where
  toField = Sql.toField . show . unwrapSha256

newtype Iso8601 = Iso8601
  { unwrapIso8601 :: Time.UTCTime
  } deriving (Eq, Ord, Show)

instance Sql.FromField Iso8601 where
  fromField field = do
    let format = "%Y-%m-%dT%H:%M:%S%QZ"
    string <- Sql.fromField field
    case Time.parseTimeM False Time.defaultTimeLocale format string of
      Nothing -> Sql.returnError Sql.ConversionFailed field
        $ "failed to parse: " <> show string
      Just utcTime -> pure $ Iso8601 utcTime

instance Sql.ToField Iso8601 where
  toField =
    Sql.toField
      . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"
      . unwrapIso8601

withConnection :: (Sql.Connection -> App a) -> App a
withConnection app = do
  pool <- Reader.asks contextPool
  Pool.withResource pool app

query :: String -> Sql.Query
query = Sql.Query . Text.pack

type App = Reader.ReaderT Context IO

runApp :: Context -> App a -> IO a
runApp = flip Reader.runReaderT

data Context = Context
  { contextConfig :: Config
  , contextPool :: Pool.Pool Sql.Connection
  }

makeContext :: Config -> IO Context
makeContext config = do
  let database = configDatabase config
  pool <- Pool.createPool
    (Sql.open database)
    Sql.close
    1
    60
    (if null database || database == ":memory:" then 1 else 8)
  pure Context
    { contextConfig = config
    , contextPool = pool
    }

data Config = Config
  { configDatabase :: String
  , configHelp :: Bool
  , configPort :: Warp.Port
  , configVersion :: Bool
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { configDatabase = "monadoc.sqlite3"
  , configHelp = False
  , configPort = 4444
  , configVersion = False
  }

getConfig :: IO Config
getConfig = do
  arguments <- Environment.getArgs
  let
    options =
      [ GetOpt.Option
        []
        ["database"]
        (GetOpt.ReqArg
          (\ database config -> Right config { configDatabase = database })
          "FILE")
        "sets the database file (defaults to \"monadoc.sqlite3\")"
      , GetOpt.Option
        ['h']
        ["help"]
        (GetOpt.NoArg (\ config -> Right config { configHelp = True }))
        "shows the help and exits"
      , GetOpt.Option
        []
        ["port"]
        (GetOpt.ReqArg
          (\ rawPort config -> case Read.readMaybe rawPort of
            Nothing -> Left $ "invalid port: " <> show rawPort
            Just port -> Right config { configPort = port })
          "NUMBER")
        "sets the server port (defaults to 4444)"
      , GetOpt.Option
        ['v']
        ["version"]
        (GetOpt.NoArg (\ config -> Right config { configVersion = True }))
        "shows the version number and exits"
      ]
    (funs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute options arguments
  Monad.forM_ args $ \ arg ->
    IO.hPutStrLn IO.stderr $ "WARNING: argument `" <> arg <> "' not expected"
  Monad.forM_ opts $ \ opt ->
    IO.hPutStrLn IO.stderr $ "WARNING: option `" <> opt <> "' not recognized"
  Monad.forM_ errs $ \ err ->
    IO.hPutStr IO.stderr $ "ERROR: " <> err
  Monad.unless (null errs) Exit.exitFailure
  config <- case Monad.foldM (flip ($)) defaultConfig funs of
    Left err -> do
      IO.hPutStrLn IO.stderr $ "ERROR: " <> err
      Exit.exitFailure
    Right cfg -> pure cfg
  let version = Version.showVersion Package.version
  Monad.when (configHelp config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo (unwords [name, "version", version]) options
    Exit.exitSuccess
  Monad.when (configVersion config) $ do
    putStrLn version
    Exit.exitSuccess
  pure config

server :: App ()
server = do
  context <- Reader.ask
  Trans.lift
    . Warp.run (configPort $ contextConfig context)
    $ \ _ respond -> respond $ Wai.responseLBS Http.ok200 [] LazyByteString.empty

worker :: App ()
worker = Monad.forever $ do
  Trans.lift $ Concurrent.threadDelay 1000000
