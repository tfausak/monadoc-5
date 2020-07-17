module Monadoc.Data.OptionsSpec where

import qualified Monadoc.Data.Options as Options
import qualified System.Console.GetOpt as GetOpt
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Data.Options" $ do

  Test.describe "options" $ do

    Test.it "has a --help option" $ do
      let
        f :: GetOpt.OptDescr a -> [String]
        f (GetOpt.Option _ x _ _) = x
      concatMap f Options.options `Test.shouldSatisfy` elem "help"
