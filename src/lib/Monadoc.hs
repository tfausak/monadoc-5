module Monadoc
  ( monadoc
  )
where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Monadoc.Console as Console
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Options as Options
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Main as Main
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

monadoc :: IO ()
monadoc = do
  config <- getConfig
  Console.info $ unwords
    [ "Starting Monadoc version"
    , Version.string
    , "commit"
    , Maybe.fromMaybe "unknown" Commit.hash
    , "..."
    ]
  context <- Context.fromConfig config
  App.run context Main.run

getConfig :: IO Config.Config
getConfig = do
  arguments <- Environment.getArgs
  let
    (funs, args, opts, errs) =
      GetOpt.getOpt' GetOpt.Permute Options.options arguments
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
    let
      extra = case Commit.hash of
        Nothing -> []
        Just hash -> ["commit", hash]
    putStr $ GetOpt.usageInfo
      (unwords $ [name, "version", Version.string] <> extra)
      Options.options
    Exit.exitSuccess
  Monad.when (Config.version config) $ do
    putStrLn $ Version.string <> case Commit.hash of
      Nothing -> ""
      Just hash -> "-" <> hash
    Exit.exitSuccess
  pure config
