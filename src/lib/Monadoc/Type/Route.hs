module Monadoc.Type.Route
  ( Route(..)
  )
where

data Route
  = Favicon
  | HealthCheck
  | Index
  | Logo
  | Robots
  | Tachyons
  | Throw
  deriving (Eq, Show)
