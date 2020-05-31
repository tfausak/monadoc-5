module Monadoc ( monadoc ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Pool as Pool
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
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
  environment <- makeEnvironment config
  Async.race_ (server environment) worker

data Environment = Environment
  { environmentConfig :: Config
  , environmentPool :: Pool.Pool Sql.Connection
  }

makeEnvironment :: Config -> IO Environment
makeEnvironment config = do
  pool <- Pool.createPool
    (Sql.open $ configDatabase config)
    Sql.close
    1
    60
    1
  pure Environment
    { environmentConfig = config
    , environmentPool = pool
    }

data Config = Config
  { configDatabase :: String
  , configHelp :: Bool
  , configPort :: Warp.Port
  , configVersion :: Bool
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { configDatabase = ":memory:"
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
        "sets the database file (defaults to \":memory:\")"
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

server :: Environment -> IO ()
server environment = Warp.run (configPort $ environmentConfig environment)
  $ \ _ respond -> respond $ Wai.responseLBS Http.ok200 [] LazyByteString.empty

worker :: IO ()
worker = Monad.forever $ do
  Concurrent.threadDelay 1000000
