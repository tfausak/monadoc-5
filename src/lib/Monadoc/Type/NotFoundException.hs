module Monadoc.Type.NotFoundException where

import qualified Control.Monad.Catch as Exception
import Monadoc.Prelude

data NotFoundException
  = NotFoundException
  deriving (Eq, Show)

instance Exception.Exception NotFoundException
