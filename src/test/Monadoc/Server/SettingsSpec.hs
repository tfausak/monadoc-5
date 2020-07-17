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
      let cfg = testConfig { Config.host = "1.2.3.4" }
      ctx <- Monadoc.configToContext cfg
      let settings = Settings.fromContext ctx
      Warp.getHost settings `shouldBe` Config.host cfg

    it "sets the port" $ do
      let cfg = testConfig { Config.database = ":memory:" }
      ctx <- Monadoc.configToContext cfg
      let settings = Settings.fromContext ctx
      Warp.getPort settings `shouldBe` Config.port cfg
