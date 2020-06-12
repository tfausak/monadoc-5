module Monadoc.Type.Route
  ( Route(..)
  )
where

data Route
  = Favicon
  | HealthCheck
  | Index
  | Robots
  | Tachyons
  | Throw
  deriving (Eq, Show)
