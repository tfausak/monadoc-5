module Monadoc.Type.RevisionSpec where

import qualified Monadoc.Type.Revision as Revision
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Revision" $ do

  Test.describe "increment" $ do

    Test.it "increases by one" $ do
      Revision.increment Revision.zero `Test.shouldBe` Revision.fromWord 1

  Test.describe "toString" $ do

    Test.it "renders just the number" $ do
      Revision.toString Revision.zero `Test.shouldBe` "0"
