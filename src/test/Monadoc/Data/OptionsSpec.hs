module Monadoc.Data.OptionsSpec where

import qualified Monadoc.Data.Options as Options
import qualified System.Console.GetOpt as GetOpt
import Test.Hspec
import Monadoc.Prelude

spec :: Spec
spec = describe "Monadoc.Data.Options" $ do

  describe "options" $ do

    it "has a --help option" $ do
      let
        f :: GetOpt.OptDescr a -> [String]
        f (GetOpt.Option _ x _ _) = x
      concatMap f Options.options `shouldSatisfy` elem "help"
