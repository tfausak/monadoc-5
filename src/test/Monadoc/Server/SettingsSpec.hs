module Monadoc.Server.SettingsSpec where

import qualified Monadoc
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Server.Settings" $ do

  Test.describe "fromConfig" $ do

    Test.it "sets the host" $ do
      let config = Test.config { Config.host = "1.2.3.4" }
      context <- Monadoc.configToContext config
      let settings = Settings.fromContext context
      Warp.getHost settings `Test.shouldBe` Config.host config

    Test.it "sets the port" $ do
      let config = Test.config { Config.database = ":memory:" }
      context <- Monadoc.configToContext config
      let settings = Settings.fromContext context
      Warp.getPort settings `Test.shouldBe` Config.port config
