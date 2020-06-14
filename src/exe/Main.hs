module Main
  ( main
  )
where

import qualified GHC.Stack as Stack
import qualified Monadoc

main :: Stack.HasCallStack => IO ()
main = Monadoc.monadoc
