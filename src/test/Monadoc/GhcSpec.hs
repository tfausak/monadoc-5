module Monadoc.GhcSpec where

import qualified Data.Either as Either
import qualified GHC.LanguageExtensions.Type as Ghc
import qualified Monadoc.Ghc as Ghc
import Test

spec :: Spec
spec = describe "Monadoc.Ghc" $ do

  describe "parse" $ do

    it "parses an empty module" $ do
      result <- Ghc.parse [] "" "module M where"
      result `shouldSatisfy` Either.isRight

    it "fails to parse an invalid module" $ do
      result <- Ghc.parse [] "" "module"
      result `shouldSatisfy` Either.isLeft

    it "fails without required extension" $ do
      result <- Ghc.parse [] "" "x# = ()"
      result `shouldSatisfy` Either.isLeft

    it "succeeds with required extension" $ do
      result <- Ghc.parse [Ghc.MagicHash] "" "x# = ()"
      result `shouldSatisfy` Either.isRight
