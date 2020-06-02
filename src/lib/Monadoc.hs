{-# LANGUAGE RankNTypes #-}

module Monadoc ( monadoc ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Fixed as Fixed
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Time as Time
import qualified Data.Typeable as Typeable
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified GHC.Stack as Stack
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

monadoc :: Stack.HasCallStack => IO ()
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
  , Migration
    { migrationIso8601 = makeIso8601 2020 6 2 13 43 0
    , migrationQuery = query
      " create table blobs \
      \( octets blob not null \
      \, sha256 text not null primary key \
      \, size integer not null )"
    }
  , Migration
    { migrationIso8601 = makeIso8601 2020 6 2 13 50 0
    , migrationQuery = query
      " create table cache \
      \( etag text not null \
      \, sha256 text not null \
      \, url text not null primary key )"
    }
  ]

newtype Etag = Etag
  { unwrapEtag :: ByteString.ByteString
  } deriving (Eq, Show)

instance Sql.FromField Etag where
  fromField = fromFieldVia $ fmap Etag . Read.readMaybe

fromFieldVia
  :: (Sql.FromField a, Show a, Typeable.Typeable b)
  => (a -> Maybe b) -> Sql.FieldParser b
fromFieldVia convert field = do
  before <- Sql.fromField field
  case convert before of
    Nothing -> Sql.returnError Sql.ConversionFailed field $ "failed to convert: " <>  show before
    Just after -> pure after

instance Sql.ToField Etag where
  toField = Sql.toField . show . unwrapEtag

newtype Url = Url
  { unwrapUrl :: Uri.URI
  } deriving (Eq, Show)

instance Sql.FromField Url where
  fromField = fromFieldVia $ fmap Url . Uri.parseURI

instance Sql.ToField Url where
  toField = Sql.toField . ($ "") . Uri.uriToString id . unwrapUrl

newtype Octets = Octets
  { unwrapOctets :: ByteString.ByteString
  } deriving (Eq, Show)

instance Sql.FromField Octets where
  fromField = fmap Octets . Sql.fromField

instance Sql.ToField Octets where
  toField = Sql.toField . unwrapOctets

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
  } deriving (Eq, Show)

instance Sql.FromField Sha256 where
  fromField = fromFieldVia $ fmap Sha256 . Read.readMaybe

instance Sql.ToField Sha256 where
  toField = Sql.toField . show . unwrapSha256

newtype Iso8601 = Iso8601
  { unwrapIso8601 :: Time.UTCTime
  } deriving (Eq, Ord, Show)

instance Sql.FromField Iso8601 where
  fromField = fromFieldVia
    $ fmap Iso8601
    . Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

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

type App a = Stack.HasCallStack => Reader.ReaderT Context IO a

runApp :: Stack.HasCallStack => Context -> App a -> IO a
runApp = flip Reader.runReaderT

data SomeExceptionWithCallStack
  = SomeExceptionWithCallStack Exception.SomeException Stack.CallStack
  deriving Show

instance Exception.Exception SomeExceptionWithCallStack where
  displayException (SomeExceptionWithCallStack e s) = unlines
    [ Exception.displayException e
    , Stack.prettyCallStack s
    ]

addCallStack
  :: Stack.HasCallStack
  => Exception.SomeException
  -> Exception.SomeException
addCallStack e = case Exception.fromException e of
  Just (SomeExceptionWithCallStack _ _) -> e
  Nothing -> Exception.toException $ SomeExceptionWithCallStack e Stack.callStack

-- removeCallStack :: Exception.SomeException -> Exception.SomeException
-- removeCallStack e = case Exception.fromException e of
--   Just (SomeExceptionWithCallStack f _) -> removeCallStack f
--   Nothing -> e

throwWithCallStack
  :: (Stack.HasCallStack, Exception.Exception e, Exception.MonadThrow m)
  => e -> m a
throwWithCallStack = Exception.throwM . addCallStack . Exception.toException

data Context = Context
  { contextConfig :: Config
  , contextManager :: Client.Manager
  , contextPool :: Pool.Pool Sql.Connection
  }

makeContext :: Config -> IO Context
makeContext config = do
  manager <- Tls.newTlsManager
  let database = configDatabase config
  pool <- Pool.createPool
    (Sql.open database)
    Sql.close
    1
    60
    (if null database || database == ":memory:" then 1 else 8)
  pure Context
    { contextConfig = config
    , contextManager = manager
    , contextPool = pool
    }

data Config = Config
  { configDatabase :: String
  , configHelp :: Bool
  , configHost :: Warp.HostPreference
  , configPort :: Warp.Port
  , configVersion :: Bool
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { configDatabase = "monadoc.sqlite3"
  , configHelp = False
  , configHost = String.fromString "127.0.0.1"
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
        "sets the database file (defaults to monadoc.sqlite3)"
      , GetOpt.Option
        ['h']
        ["help"]
        (GetOpt.NoArg (\ config -> Right config { configHelp = True }))
        "shows the help and exits"
      , GetOpt.Option
        []
        ["host"]
        (GetOpt.ReqArg
          (\ host config -> Right config { configHost = String.fromString host })
          "STRING")
        "sets the server host (defaults to 127.0.0.1)"
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
  Monad.when (configHelp config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo (unwords [name, "version", version]) options
    Exit.exitSuccess
  Monad.when (configVersion config) $ do
    putStrLn version
    Exit.exitSuccess
  pure config

version :: String
version = Version.showVersion Package.version

server :: App ()
server = do
  context <- Reader.ask
  Trans.lift
    . Warp.runSettings (makeSettings $ contextConfig context)
    $ \ request respond -> runApp context $ do
      let method = fromUtf8 $ Wai.requestMethod request
      let path = fmap Text.unpack $ Wai.pathInfo request
      case (method, path) of
        ("GET", ["health-check"]) ->
          Trans.lift . respond $ statusResponse Http.ok200 []
        ("GET", ["throw"]) ->
          throwWithCallStack $ userError "oh no"
        _ ->
          Trans.lift . respond $ statusResponse Http.notFound404 []

makeSettings :: Config -> Warp.Settings
makeSettings config =
  Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (configHost config)
    . Warp.setLogger logger
    . Warp.setOnException onException
    . Warp.setOnExceptionResponse onExceptionResponse
    . Warp.setPort (configPort config)
    $ Warp.setServerName serverName Warp.defaultSettings

beforeMainLoop :: Config -> IO ()
beforeMainLoop config = putStrLn $ unwords
  ["Listening on", show $ configHost config, "port", show $ configPort config]

logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ = putStrLn $ unwords
  [ show $ Http.statusCode status
  , fromUtf8 $ Wai.requestMethod request
  , fromUtf8 $ Wai.rawPathInfo request <> Wai.rawQueryString request
  ]

onException :: Maybe Wai.Request -> Exception.SomeException -> IO ()
onException _ someException@(Exception.SomeException exception) =
  Monad.when (Warp.defaultShouldDisplayException someException)
    . IO.hPutStrLn IO.stderr
    $ Exception.displayException exception

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = statusResponse Http.internalServerError500 []

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers = stringResponse status headers $ unwords
  [show $ Http.statusCode status, fromUtf8 $ Http.statusMessage status]

stringResponse :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
stringResponse status headers string = Wai.responseLBS
  status
  ((Http.hContentType, toUtf8 "text/plain; charset=utf-8") : headers)
  (LazyByteString.fromStrict $ toUtf8 string)

fromUtf8 :: ByteString.ByteString -> String
fromUtf8 = Text.unpack . Text.decodeUtf8With Text.lenientDecode

toUtf8 :: String -> ByteString.ByteString
toUtf8 = Text.encodeUtf8 . Text.pack

serverName :: ByteString.ByteString
serverName = toUtf8 $ "monadoc-" <> version

worker :: App ()
worker = Monad.forever $ do
  Trans.lift $ putStrLn "updating hackage index"
  let url = "https://hackage.haskell.org/01-index.tar.gz"
  result <- withConnection $ \ connection -> Trans.lift $ Sql.query connection
    (query "select etag, sha256 from cache where url = ?") [url]
  contents <- case result of
    [] -> do
      Trans.lift $ putStrLn "index is not cached"
      request <- Client.parseRequest url
      manager <- Reader.asks contextManager
      response <- Trans.lift $ Client.httpLbs request manager
      Monad.when (Client.responseStatus response /= Http.ok200)
        . throwWithCallStack
        . userError
        $ show response
      let
        body = LazyByteString.toStrict $ Client.responseBody response
        sha256 = Sha256 $ Crypto.hash body
        etag = Etag . Maybe.fromMaybe ByteString.empty . lookup Http.hETag
          $ Client.responseHeaders response
      Trans.lift $ putStrLn "got index, caching response"
      withConnection $ \ connection -> Trans.lift $ do
        Sql.execute connection
          (query "insert into blobs (octets, sha256, size) values (?, ?, ?) on conflict do nothing")
          (Octets body, sha256, Size $ ByteString.length body)
        Sql.execute connection
          (query "insert into cache (etag, sha256, url) values (?, ?, ?)")
          (etag, sha256, url)
      pure body
    (etag, sha256) : _ -> do
      Trans.lift $ putStrLn "index is cached"
      request <- fmap (addRequestHeader Http.hIfNoneMatch $ unwrapEtag etag) $ Client.parseRequest url
      manager <- Reader.asks contextManager
      response <- Trans.lift $ Client.httpLbs request manager
      case Http.statusCode $ Client.responseStatus response of
        200 -> do
          Trans.lift $ putStrLn "index has changed"
          let
            body = LazyByteString.toStrict $ Client.responseBody response
            newSha256 = Sha256 $ Crypto.hash body
            newEtag = Etag . Maybe.fromMaybe ByteString.empty . lookup Http.hETag
              $ Client.responseHeaders response
          Trans.lift $ putStrLn "got index, caching response"
          withConnection $ \ connection -> Trans.lift $ do
            Sql.execute connection
              (query "insert into blobs (octets, sha256) values (?, ?, ?) on conflict do nothing")
              (Octets body, newSha256, Size $ ByteString.length body)
            Sql.execute connection
              (query "insert into cache (etag, sha256, url) values (?, ?, ?)")
              (newEtag, newSha256, url)
          pure body
        304 -> do
          Trans.lift $ putStrLn "index has not changed"
          rows <- withConnection $ \ connection -> Trans.lift $ Sql.query connection
            (query "select octets from blobs where sha256 = ?") [sha256 :: Sha256]
          case rows of
            [] -> throwWithCallStack $ userError "missing index blob"
            row : _ -> pure . unwrapOctets $ Sql.fromOnly row
        _ -> throwWithCallStack . userError $ show response
  Trans.lift . putStrLn $ "index size: " <> show (ByteString.length contents)
  Trans.lift $ Concurrent.threadDelay 60000000

newtype Size = Size
  { unwrapSize :: Int
  } deriving (Eq, Show)

instance Sql.ToField Size where
  toField = Sql.toField . unwrapSize

addRequestHeader :: Http.HeaderName -> ByteString.ByteString -> Client.Request -> Client.Request
addRequestHeader name value request = request { Client.requestHeaders = (name, value) : Client.requestHeaders request }
