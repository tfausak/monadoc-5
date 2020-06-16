module Test.Extra
  ( config
  , makeContext
  )
where

import qualified Monadoc
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context

config :: Config.Config
config =
  Config.initial { Config.database = ":memory:", Config.url = "http://test" }

makeContext :: IO (Context.Context ())
makeContext = Monadoc.configToContext config
