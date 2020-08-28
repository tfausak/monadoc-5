module Monadoc.Utility.CabalSpec where

import qualified Data.Either as Either
import qualified Monadoc.Utility.Cabal as Cabal
import Test.Hspec
import Monadoc.Prelude

spec :: Spec
spec = describe "Monadoc.Utility.Cabal" $ do

  describe "parse" $ do

    it "parses an empty package" $ do
      Cabal.parse "name:x\nversion:0" `shouldSatisfy` Either.isRight

    it "fails to parse an invalid package" $ do
      Cabal.parse "" `shouldSatisfy` Either.isLeft
