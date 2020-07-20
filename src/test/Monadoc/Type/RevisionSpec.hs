module Monadoc.Type.RevisionSpec where

import qualified Monadoc.Type.Revision as Revision
import Test

spec :: Spec
spec = describe "Monadoc.Type.Revision" $ do

  describe "increment" $ do

    it "increases by one" $ do
      Revision.increment Revision.zero `shouldBe` Revision.fromWord 1

  describe "toString" $ do

    it "renders just the number" $ do
      Revision.toString Revision.zero `shouldBe` "0"
