module Monadoc.Test where

import qualified Monadoc
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context

makeContext :: IO (Context.Context ())
makeContext = Monadoc.configToContext testConfig

testConfig :: Config.Config
testConfig =
  Config.initial { Config.database = ":memory:", Config.url = "http://test" }
