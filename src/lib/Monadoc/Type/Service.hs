module Monadoc.Type.Service where

data Service
  = Server
  | Worker
  deriving (Eq, Ord, Show)
