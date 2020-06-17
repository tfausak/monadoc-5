module Monadoc.Type.ConfigSpec
  ( spec
  )
where

import qualified Monadoc.Type.Config as Config
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Config" $ do

  Test.describe "initial" $ do

    Test.it "does not show the help" $ do
      Config.help Config.initial `Test.shouldBe` False

    Test.it "does not show the version" $ do
      Config.version Config.initial `Test.shouldBe` False

    Test.it "does not bind all hosts" $ do
      Config.host Config.initial `Test.shouldBe` "127.0.0.1"
