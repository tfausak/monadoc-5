module Monadoc.Data.OptionsSpec
  ( spec
  )
where

import qualified Monadoc.Data.Options as Options
import qualified System.Console.GetOpt as GetOpt
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Data.Options" $ do

  Hspec.describe "options" $ do

    Hspec.it "has a --help option" $ do
      let
        f :: GetOpt.OptDescr a -> [String]
        f (GetOpt.Option _ x _ _) = x
      concatMap f Options.options `Hspec.shouldSatisfy` elem "help"
