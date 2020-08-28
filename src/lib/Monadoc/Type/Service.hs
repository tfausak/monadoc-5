module Monadoc.Type.Service where
import Monadoc.Prelude

data Service
  = Server
  | Worker
  deriving (Eq, Ord, Show)
