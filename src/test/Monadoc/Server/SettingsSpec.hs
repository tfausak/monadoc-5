module Monadoc.Server.SettingsSpec
  ( spec
  )
where

import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Monadoc
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Server.Settings" $ do

  Hspec.describe "fromConfig" $ do

    Hspec.it "sets the host" $ do
      let
        config = Config.initial
          { Config.database = ":memory:"
          , Config.host = "1.2.3.4"
          }
      context <- Monadoc.configToContext config
      let settings = Settings.fromContext context
      Warp.getHost settings `Hspec.shouldBe` Config.host config

    Hspec.it "sets the port" $ do
      let
        config =
          Config.initial { Config.database = ":memory:", Config.port = 1234 }
      context <- Monadoc.configToContext config
      let settings = Settings.fromContext context
      Warp.getPort settings `Hspec.shouldBe` Config.port config
