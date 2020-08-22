module Monadoc where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Pool as Pool
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Utility.Console as Console
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Options as Options
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Main as Main
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.ConfigResult as ConfigResult
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Client.TLS as Tls
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

-- | The main app entrypoint. This is what the executable runs.
monadoc :: IO ()
monadoc = do
  Monad.forM_ [IO.stderr, IO.stdout] $ \handle -> do
    IO.hSetBuffering handle IO.LineBuffering
    IO.hSetEncoding handle IO.utf8
  config <- getConfig
  Console.info $ mconcat
    [ "\x1f516 Starting Monadoc version "
    , Version.string
    , case Commit.hash of
      Nothing -> ""
      Just hash -> " commit " <> hash
    , " ..."
    ]
  context <- configToContext config
  Exception.finally (App.run context Main.run)
    . Pool.destroyAllResources
    $ Context.pool context

getConfig :: IO Config.Config
getConfig = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  case argumentsToConfigResult name arguments of
    ConfigResult.Failure errs -> do
      mapM_ (IO.hPutStr IO.stderr) errs
      Exit.exitFailure
    ConfigResult.ExitWith msg -> do
      putStr msg
      Exit.exitSuccess
    ConfigResult.Success msgs config -> do
      mapM_ (IO.hPutStr IO.stderr) msgs
      pure config

-- | Parses command-line arguments into a config.
argumentsToConfigResult
  :: String
  -- ^ The program name, usually from 'Environment.getProgName'.
  -> [String]
  -- ^ The command-line arguments, usually from 'Environment.getArgs'.
  -> ConfigResult.ConfigResult
argumentsToConfigResult name arguments =
  let
    (funs, args, opts, errs) =
      GetOpt.getOpt' GetOpt.Permute Options.options arguments
    helpHash = case Commit.hash of
      Nothing -> []
      Just hash -> ["commit", hash]
    help = GetOpt.usageInfo
      (unwords $ [name, "version", Version.string] <> helpHash)
      Options.options
    versionHash = case Commit.hash of
      Nothing -> ""
      Just hash -> "-" <> hash
    version = Version.string <> versionHash <> "\n"
    formatArg arg = "WARNING: argument `" <> arg <> "' not expected\n"
    formatOpt opt = "WARNING: option `" <> opt <> "' not recognized\n"
    warnings = fmap formatArg args <> fmap formatOpt opts
  in case NonEmpty.nonEmpty errs of
    Just es -> ConfigResult.Failure $ fmap ("ERROR: " <>) es
    Nothing -> case Monad.foldM (flip ($)) Config.initial funs of
      Left err -> ConfigResult.Failure . pure $ "ERROR: " <> err <> "\n"
      Right config -> if Config.help config
        then ConfigResult.ExitWith help
        else if Config.version config
          then ConfigResult.ExitWith version
          else ConfigResult.Success warnings config

-- | Converts a config into a context. This involves acquiring any resources
-- described in the config.
configToContext :: Config.Config -> IO (Context.Context ())
configToContext config = do
  manager <- Tls.newTlsManager
  let database = Config.database config
  maxResources <- if isInMemory database then pure 1 else getMaxResources
  pool <- Pool.createPool
    (Sql.open database)
    Sql.close
    stripeCount
    idleTime
    maxResources
  pure Context.Context
    { Context.config = config
    , Context.manager = manager
    , Context.pool = pool
    , Context.request = ()
    }

stripeCount :: Int
stripeCount = 1

idleTime :: Time.NominalDiffTime
idleTime = 60

getMaxResources :: IO Int
getMaxResources = fmap (max 1) Concurrent.getNumCapabilities

isInMemory :: FilePath -> Bool
isInMemory database = case database of
  "" -> True
  ":memory:" -> True
  _ -> False

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc" $ do

  Hspec.describe "argumentsToConfigResult" $ do

    Hspec.it "returns the default with no arguments" $ do
      Monadoc.argumentsToConfigResult "x" []
        `Hspec.shouldBe` ConfigResult.Success [] Config.initial

    Hspec.it "shows the help" $ do
      Monadoc.argumentsToConfigResult "x" ["--help"]
        `Hspec.shouldSatisfy` isExitWith

    Hspec.it "shows the version" $ do
      Monadoc.argumentsToConfigResult "x" ["--version"]
        `Hspec.shouldSatisfy` isExitWith

    Hspec.it "fails when given disallowed argument" $ do
      Monadoc.argumentsToConfigResult "x" ["--help=0"]
        `Hspec.shouldSatisfy` isFailure

    Hspec.it "warns when given unexpected parameters" $ do
      case Monadoc.argumentsToConfigResult "x" ["y"] of
        ConfigResult.Success msgs _ -> msgs `Hspec.shouldSatisfy` not . null
        result -> result `Hspec.shouldSatisfy` isSuccess

    Hspec.it "warns when given unknown options" $ do
      case Monadoc.argumentsToConfigResult "x" ["-y"] of
        ConfigResult.Success msgs _ -> msgs `Hspec.shouldSatisfy` not . null
        result -> result `Hspec.shouldSatisfy` isSuccess

    Hspec.it "sets the port" $ do
      case Monadoc.argumentsToConfigResult "x" ["--port=123"] of
        ConfigResult.Success _ cfg -> Config.port cfg `Hspec.shouldBe` 123
        result -> result `Hspec.shouldSatisfy` isSuccess

  Hspec.describe "configToContext" $ do

    Hspec.it "works" $ do
      Hspec.pending
      -- let cfg = testConfig
      -- ctx <- Monadoc.configToContext cfg
      -- Context.config ctx `Hspec.shouldBe` cfg

isExitWith :: ConfigResult.ConfigResult -> Bool
isExitWith configResult = case configResult of
  ConfigResult.ExitWith _ -> True
  _ -> False

isFailure :: ConfigResult.ConfigResult -> Bool
isFailure configResult = case configResult of
  ConfigResult.Failure _ -> True
  _ -> False

isSuccess :: ConfigResult.ConfigResult -> Bool
isSuccess configResult = case configResult of
  ConfigResult.Success _ _ -> True
  _ -> False
