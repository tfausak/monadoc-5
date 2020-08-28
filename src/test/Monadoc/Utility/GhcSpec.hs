module Monadoc.Utility.GhcSpec where

import qualified Data.Either as Either
import qualified GHC.LanguageExtensions.Type as Ext
import Monadoc.Prelude
import qualified Monadoc.Utility.Ghc as Ghc
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Utility.Ghc" $ do

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

    it "works with CPP pragma" $ do
      result <- Ghc.parse [] "" "{-# language CPP #-}\n#"
      result `shouldSatisfy` Either.isRight

    it "does not throw impure CPP errors" $ do
      result <- Ghc.parse [(True, Ext.Cpp)] "" "#error"
      result `shouldSatisfy` Either.isLeft

    it "works with implied extensions" $ do
      result <- Ghc.parse
        [(True, Ext.RankNTypes)]
        ""
        "f :: forall a . a -> a\nf a = a"
      result `shouldSatisfy` Either.isRight
