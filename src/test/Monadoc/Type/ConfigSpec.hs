module Monadoc.Type.ConfigSpec where

import Monadoc.Prelude
import qualified Monadoc.Type.Config as Config
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.Config" $ do

  describe "initial" $ do

    it "does not show the help" $ do
      Config.help Config.initial `shouldBe` False

    it "does not show the version" $ do
      Config.version Config.initial `shouldBe` False

    it "does not bind all hosts" $ do
      Config.host Config.initial `shouldBe` "127.0.0.1"
