#! /usr/bin/env stack
-- stack exec --package Glob --package process runghc
import qualified System.FilePath.Glob as Glob
import qualified System.IO as IO
import qualified System.Process as Process

main :: IO ()
main = do
  files <- Glob.glob "src/**/*.hs"
  mapM_ (convertNewlinesTo IO.LF) files
  Process.callProcess "brittany"
    $ "--config-file=config/brittany.yaml"
    : "--write-mode=inplace"
    : files
  mapM_ (convertNewlinesTo IO.nativeNewline) files

convertNewlinesTo :: IO.Newline -> FilePath -> IO ()
convertNewlinesTo newline file = do
  contents <- IO.withFile file IO.ReadMode $ \handle -> do
    IO.hSetNewlineMode handle IO.universalNewlineMode
    contents <- IO.hGetContents handle
    () <- seq (length contents) $ pure ()
    pure contents
  IO.withFile file IO.WriteMode $ \handle -> do
    IO.hSetNewlineMode handle $ IO.NewlineMode newline newline
    IO.hPutStr handle contents
