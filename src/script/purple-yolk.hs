#! /usr/bin/env stack
-- stack exec --package process runghc
import qualified System.Process as Process

main :: IO ()
main = Process.callProcess
  "stack"
  [ "--color=never"
  , "ghci"
  , "--ghc-options=-ddump-json"
  , "--ghc-options=-fdefer-type-errors"
  , "--ghc-options=-fmax-relevant-binds=0"
  , "--ghc-options=-fno-diagnostics-show-caret"
  , "--ghc-options=-fobject-code"
  , "--ghc-options=-funclutter-valid-hole-fits"
  , "--ghc-options=-j4"
  , "--ghc-options=-O0"
  , "--main-is=monadoc:test"
  , "--test"
  ]
