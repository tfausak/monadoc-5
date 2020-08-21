module Monadoc.Type.Cabal.Version where

import qualified Data.List as List
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

newtype Version = Version Cabal.Version deriving (Eq, Ord, Show)

instance Sql.ToField Version where
  toField = Sql.toField . toString

fromCabal :: Cabal.Version -> Version
fromCabal = Version

fromString :: String -> Maybe Version
fromString = fmap fromCabal . Cabal.simpleParsec

toCabal :: Version -> Cabal.Version
toCabal (Version cabal) = cabal

toString :: Version -> String
toString = List.intercalate "." . fmap show . Cabal.versionNumbers . toCabal

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Cabal.Version" $ do

  Hspec.it "needs tests" Hspec.pending
