{-# LANGUAGE RankNTypes #-}

module Monadoc
  ( monadoc
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified GHC.Stack as Stack
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Migrations as Migrations
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Type.Binary as Binary
import qualified Monadoc.Type.Etag as Etag
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Size as Size
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Read as Read

monadoc :: Stack.HasCallStack => IO ()
monadoc = do
  config <- getConfig
  say $ unwords
    ["Starting Monadoc version"
    , Version.string
    , "commit"
    , Maybe.fromMaybe "unknown" Commit.hash
    , "..."
    ]
  context <- makeContext config
  runApp context migrate
  Async.race_ (runApp context server) (runApp context worker)

say :: IO.MonadIO m => String -> m ()
say message = IO.liftIO $ do
  now <- Time.getCurrentTime
  let
    timestamp =
      Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" now
  putStrLn $ unwords [timestamp, message]

migrate :: App ()
migrate = withConnection $ \connection -> Trans.lift $ do
  Sql.execute_ connection $ query "pragma journal_mode = wal"
  Sql.execute_ connection
    $ query
        "create table if not exists migrations \
        \( iso8601 text not null primary key \
        \, sha256 text not null )"
  digests <- fmap Map.fromList . Sql.query_ connection $ query
    "select iso8601, sha256 from migrations"
  Monad.forM_ Migrations.migrations $ \migration -> do
    let timestamp = Migration.timestamp migration
    let actualSha256 = Migration.sha256 migration
    case Map.lookup timestamp digests of
      Nothing -> Sql.withTransaction connection $ do
        Sql.execute_ connection $ Migration.query migration
        Sql.execute
          connection
          (query "insert into migrations (iso8601, sha256) values (?, ?)")
          migration
      Just expectedSha256 ->
        Monad.when (actualSha256 /= expectedSha256)
        $ Exception.throwM MigrationMismatch.MigrationMismatch
          { MigrationMismatch.actual = actualSha256
          , MigrationMismatch.expected = expectedSha256
          , MigrationMismatch.timestamp = timestamp
          }

withConnection :: (Sql.Connection -> App a) -> App a
withConnection app = do
  pool <- Reader.asks contextPool
  Pool.withResource pool app

query :: String -> Sql.Query
query = Sql.Query . Text.pack

type App a = Stack.HasCallStack => Reader.ReaderT Context IO a

runApp :: Stack.HasCallStack => Context -> App a -> IO a
runApp = flip Reader.runReaderT

data Context = Context
  { contextConfig :: Config
  , contextManager :: Client.Manager
  , contextPool :: Pool.Pool Sql.Connection
  }

makeContext :: Config -> IO Context
makeContext config = do
  manager <- Tls.newTlsManager
  let database = configDatabase config
  capabilities <- Concurrent.getNumCapabilities
  pool <- Pool.createPool
    (Sql.open database)
    Sql.close
    1
    60
    (if null database || database == ":memory:" then 1 else capabilities + 1)
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
          (\database config -> Right config { configDatabase = database })
          "FILE"
        )
        "sets the database file (defaults to monadoc.sqlite3)"
      , GetOpt.Option
        ['h']
        ["help"]
        (GetOpt.NoArg (\config -> Right config { configHelp = True }))
        "shows the help and exits"
      , GetOpt.Option
        []
        ["host"]
        (GetOpt.ReqArg
          (\host config -> Right config { configHost = String.fromString host }
          )
          "STRING"
        )
        "sets the server host (defaults to 127.0.0.1)"
      , GetOpt.Option
        []
        ["port"]
        (GetOpt.ReqArg
          (\rawPort config -> case Read.readMaybe rawPort of
            Nothing -> Left $ "invalid port: " <> show rawPort
            Just port -> Right config { configPort = port }
          )
          "NUMBER"
        )
        "sets the server port (defaults to 4444)"
      , GetOpt.Option
        ['v']
        ["version"]
        (GetOpt.NoArg (\config -> Right config { configVersion = True }))
        "shows the version number and exits"
      ]
    (funs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute options arguments
  Monad.forM_ args $ \arg ->
    IO.hPutStrLn IO.stderr $ "WARNING: argument `" <> arg <> "' not expected"
  Monad.forM_ opts $ \opt ->
    IO.hPutStrLn IO.stderr $ "WARNING: option `" <> opt <> "' not recognized"
  Monad.forM_ errs $ \err -> IO.hPutStr IO.stderr $ "ERROR: " <> err
  Monad.unless (null errs) Exit.exitFailure
  config <- case Monad.foldM (flip ($)) defaultConfig funs of
    Left err -> do
      IO.hPutStrLn IO.stderr $ "ERROR: " <> err
      Exit.exitFailure
    Right cfg -> pure cfg
  Monad.when (configHelp config) $ do
    name <- Environment.getProgName
    let extra = case Commit.hash of
          Nothing -> []
          Just hash -> ["commit", hash]
    putStr $ GetOpt.usageInfo (unwords $ [name, "version", Version.string] <> extra) options
    Exit.exitSuccess
  Monad.when (configVersion config) $ do
    putStrLn $ Version.string <> case Commit.hash of
      Nothing -> ""
      Just hash -> "-" <> hash
    Exit.exitSuccess
  pure config

server :: App ()
server = do
  context <- Reader.ask
  Trans.lift
    . Warp.runSettings (makeSettings $ contextConfig context)
    $ \request respond -> runApp context $ do
        let method = fromUtf8 $ Wai.requestMethod request
        let path = Text.unpack <$> Wai.pathInfo request
        case (method, path) of
          ("GET", ["health-check"]) ->
            Trans.lift . respond $ statusResponse Http.ok200 []
          ("GET", ["throw"]) -> WithCallStack.throw $ userError "oh no"
          _ -> Trans.lift . respond $ statusResponse Http.notFound404 []

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
beforeMainLoop config = say $ unwords
  ["Listening on", show $ configHost config, "port", show $ configPort config]

logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ = say $ unwords
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
statusResponse status headers = stringResponse status headers
  $ unwords
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
serverName = toUtf8 $ "monadoc-" <> Version.string <> case Commit.hash of
  Nothing -> ""
  Just hash -> "-" <> hash

worker :: App ()
worker = Monad.forever $ do
  say "updating hackage index"
  let url = "https://hackage.haskell.org/01-index.tar.gz"
  result <- withConnection $ \connection -> Trans.lift $ Sql.query
    connection
    (query "select etag, sha256 from cache where url = ?")
    [url]
  contents <- case result of
    [] -> do
      say "index is not cached"
      request <- Client.parseRequest url
      manager <- Reader.asks contextManager
      response <- Trans.lift $ Client.httpLbs request manager
      Monad.when (Client.responseStatus response /= Http.ok200)
        . WithCallStack.throw
        . userError
        $ show response
      let
        body = LazyByteString.toStrict $ Client.responseBody response
        sha256 = Sha256.fromDigest $ Crypto.hash body
        etag =
          Etag.fromByteString
            . Maybe.fromMaybe ByteString.empty
            . lookup Http.hETag
            $ Client.responseHeaders response
      say "got index, caching response"
      withConnection $ \connection -> Trans.lift $ do
        Sql.execute
          connection
          (query
            "insert into blobs (octets, sha256, size) \
            \ values (?, ?, ?) on conflict (sha256) do nothing"
          )
          (Binary.fromByteString body, sha256, Size.fromInt $ ByteString.length body)
        Sql.execute
          connection
          (query
            "insert into cache (etag, sha256, url) values (?, ?, ?) \
            \ on conflict (url) do update set \
            \ etag = excluded.etag, sha256 = excluded.sha256"
          )
          (etag, sha256, url)
      pure body
    (etag, sha256) : _ -> do
      say "index is cached"
      request <- addRequestHeader Http.hIfNoneMatch (Etag.toByteString etag)
        <$> Client.parseRequest url
      manager <- Reader.asks contextManager
      response <- Trans.lift $ Client.httpLbs request manager
      case Http.statusCode $ Client.responseStatus response of
        200 -> do
          say "index has changed"
          let
            body = LazyByteString.toStrict $ Client.responseBody response
            newSha256 = Sha256.fromDigest $ Crypto.hash body
            newEtag =
              Etag.fromByteString
                . Maybe.fromMaybe ByteString.empty
                . lookup Http.hETag
                $ Client.responseHeaders response
          say "got index, caching response"
          withConnection $ \connection -> Trans.lift $ do
            Sql.execute
              connection
              (query
                "insert into blobs (octets, sha256, size) \
                \ values (?, ?, ?) on conflict (sha256) do nothing"
              )
              ( Binary.fromByteString body
              , newSha256
              , Size.fromInt $ ByteString.length body
              )
            Sql.execute
              connection
              (query
                "insert into cache (etag, sha256, url) values (?, ?, ?) \
                \ on conflict (url) do update set \
                \ etag = excluded.etag, sha256 = excluded.sha256"
              )
              (newEtag, newSha256, url)
          pure body
        304 -> do
          say "index has not changed"
          rows <- withConnection $ \connection -> Trans.lift $ Sql.query
            connection
            (query "select octets from blobs where sha256 = ?")
            [sha256 :: Sha256.Sha256]
          case rows of
            [] -> WithCallStack.throw $ userError "missing index blob"
            row : _ -> pure . Binary.toByteString $ Sql.fromOnly row
        _ -> WithCallStack.throw . userError $ show response
  say $ "index size: " <> show (ByteString.length contents)
  say
    . mappend "index entry count: "
    . show
    . length
    . Tar.foldEntries
        (\entry entries -> case Tar.entryContent entry of
          Tar.NormalFile _contents _size ->
            case FilePath.takeExtension $ Tar.entryPath entry of
              "" -> entry : entries -- preferred-versions
              ".cabal" -> entry : entries
              ".json" -> entries -- ignore
              _ ->
                Unsafe.unsafePerformIO . WithCallStack.throw . userError $ show
                  entry
          _ -> Unsafe.unsafePerformIO . WithCallStack.throw . userError $ show
            entry
        )
        []
        (Unsafe.unsafePerformIO . WithCallStack.throw)
    . Tar.read
    . Gzip.decompress
    $ LazyByteString.fromStrict contents
  Trans.lift $ Concurrent.threadDelay 60000000

addRequestHeader
  :: Http.HeaderName
  -> ByteString.ByteString
  -> Client.Request
  -> Client.Request
addRequestHeader name value request = request
  { Client.requestHeaders = (name, value) : Client.requestHeaders request
  }
