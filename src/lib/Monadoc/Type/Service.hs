module Monadoc.Type.Service
  ( Service(..)
  )
where

data Service
  = Server
  | Worker
  deriving (Eq, Ord, Show)
