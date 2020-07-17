module Monadoc.GhcSpec where

import qualified Data.Either as Either
import qualified Monadoc.Ghc as Ghc
import Test

spec :: Spec
spec = describe "Monadoc.Ghc" $ do

  describe "parse" $ do

    it "parses an empty module" $ do
      result <- Ghc.parse "" "module M where"
      result `shouldSatisfy` Either.isRight

    it "fails to parse an invalid module" $ do
      result <- Ghc.parse "" "module"
      result `shouldSatisfy` Either.isLeft
