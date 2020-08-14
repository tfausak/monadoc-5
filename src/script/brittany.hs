#! /usr/bin/env stack
-- stack exec --package Glob --package process runghc
module Main where
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified System.FilePath.Glob as Glob
import qualified System.IO as IO
import qualified System.Process as Process

main :: IO ()
main = do
  files <- Glob.glob "src/**/*.hs"
  Monad.forM_ files $ \file -> do
    contents <- IO.withFile file IO.ReadMode $ \handle -> do
      IO.hSetNewlineMode handle IO.universalNewlineMode
      contents <- IO.hGetContents handle
      putStrLn $ unwords [file, show $ length contents]
      pure contents
    IO.withFile file IO.WriteMode $ \handle -> do
      IO.hSetNewlineMode handle IO.noNewlineTranslation
      IO.hPutStr handle contents
  Process.callProcess "brittany"
    $ "--config-file=config/brittany.yaml"
    : "--write-mode=inplace"
    : files
