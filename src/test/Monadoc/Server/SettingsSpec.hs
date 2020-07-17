module Monadoc.Server.SettingsSpec where

import qualified Monadoc
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Network.Wai.Handler.Warp as Warp
import Test

spec :: Spec
spec = describe "Monadoc.Server.Settings" $ do

  describe "fromConfig" $ do

    it "sets the host" $ do
      let config = testConfig { Config.host = "1.2.3.4" }
      context <- Monadoc.configToContext config
      let settings = Settings.fromContext context
      Warp.getHost settings `shouldBe` Config.host config

    it "sets the port" $ do
      let config = testConfig { Config.database = ":memory:" }
      context <- Monadoc.configToContext config
      let settings = Settings.fromContext context
      Warp.getPort settings `shouldBe` Config.port config
