module Monadoc.GhcSpec where

import qualified Data.Either as Either
import qualified GHC.LanguageExtensions.Type as Ext
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
      result <- Ghc.parse [(True, Ext.MagicHash)] "" "x# = ()"
      result `shouldSatisfy` Either.isRight

    it "succeeds with default extension" $ do
      result <- Ghc.parse [] "" "data X = X {}"
      result `shouldSatisfy` Either.isRight

    it "fails with default extension disabled" $ do
      result <- Ghc.parse
        [(False, Ext.TraditionalRecordSyntax)]
        ""
        "data X = X {}"
      result `shouldSatisfy` Either.isLeft

    it "works with CPP" $ do
      result <- Ghc.parse
        [(True, Ext.Cpp)]
        ""
        "#ifdef NOT_DEFINED\n\
        \invalid# = True\n\
        \#else\n\
        \module M where\n\
        \#endif"
      result `shouldSatisfy` Either.isRight
