#! /usr/bin/env stack
-- stack exec --package process runghc
import qualified System.Process as Process

main :: IO ()
main = Process.callProcess
  "ghcid"
  [ "--command=stack ghci"
  , "--reload=monadoc.cabal"
  , "--test=:main --services server"
  , "--warnings"
  ]
