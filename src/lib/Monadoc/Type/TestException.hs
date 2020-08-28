module Monadoc.Type.TestException where

import qualified Control.Monad.Catch as Exception
import Monadoc.Prelude

data TestException
  = TestException
  deriving (Eq, Show)

instance Exception.Exception TestException
