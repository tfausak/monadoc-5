module Monadoc.Type.Cabal.VersionRange where

import qualified Data.Maybe as Maybe
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import qualified Test.Hspec as Hspec

newtype VersionRange
  = VersionRange Cabal.VersionRange
  deriving (Eq, Show)

instance Sql.ToField VersionRange where
  toField = Sql.toField . toString

fromCabal :: Cabal.VersionRange -> VersionRange
fromCabal = VersionRange

fromString :: String -> Maybe VersionRange
fromString = fmap fromCabal . Cabal.simpleParsec

toCabal :: VersionRange -> Cabal.VersionRange
toCabal (VersionRange cabal) = cabal

toString :: VersionRange -> String
toString = Cabal.prettyShow . toCabal

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Cabal.VersionRange" $ do

  Hspec.describe "fromString" $ do

    Hspec.it "works" $ do
      fromString "> 0" `Hspec.shouldSatisfy` Maybe.isJust

  Hspec.describe "toString" $ do

    Hspec.it "works" $ do
      let string = ">0"
      Just versionRange <- pure $ fromString string
      toString versionRange `Hspec.shouldBe` string
