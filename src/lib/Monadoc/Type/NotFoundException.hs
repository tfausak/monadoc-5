module Monadoc.Type.NotFoundException
  ( NotFoundException(..)
  )
where

import qualified Control.Monad.Catch as Exception

data NotFoundException
  = NotFoundException
  deriving (Eq, Show)

instance Exception.Exception NotFoundException