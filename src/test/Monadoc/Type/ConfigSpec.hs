module Monadoc.Type.ConfigSpec
  ( spec
  )
where

import qualified Monadoc.Type.Config as Config
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Config" $ do

  Hspec.describe "initial" $ do

    Hspec.it "does not show the help" $ do
      Config.help Config.initial `Hspec.shouldBe` False

    Hspec.it "does not show the version" $ do
      Config.version Config.initial `Hspec.shouldBe` False

    Hspec.it "does not bind all hosts" $ do
      Config.host Config.initial `Hspec.shouldBe` "127.0.0.1"
