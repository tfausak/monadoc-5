#! /usr/bin/env stack
-- stack exec --package Glob --package process runghc
module Main where
import qualified System.FilePath.Glob as Glob
import qualified System.Process as Process

main :: IO ()
main = do
  files <- Glob.glob "src/**/*.hs"
  Process.callProcess "hlint" $ "lint" : "--hint=config/hlint.yaml" : files