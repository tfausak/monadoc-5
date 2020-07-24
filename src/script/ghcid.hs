#! /usr/bin/env stack
-- stack exec --package process runghc
module Main where
import qualified System.Process as Process

main :: IO ()
main = Process.callProcess
  "ghcid"
  [ "--command=stack ghci"
  , "--reload=monadoc.cabal"
  , "--test=:main --services server"
  , "--warnings"
  ]
