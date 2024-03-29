module Monadoc.Data.OptionsSpec where

import qualified Monadoc.Data.Options as Options
import Monadoc.Prelude
import qualified System.Console.GetOpt as GetOpt
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Data.Options" <| do

  describe "options" <| do

    it "has a --help option" <| do
      let
        f :: GetOpt.OptDescr a -> [String]
        f (GetOpt.Option _ x _ _) = x
      foldMap f Options.options `shouldSatisfy` elem "help"
