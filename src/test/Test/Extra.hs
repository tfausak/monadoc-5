module Test.Extra
  ( config
  )
where

import qualified Monadoc.Type.Config as Config

config :: Config.Config
config =
  Config.initial { Config.database = ":memory:", Config.url = "http://test" }
