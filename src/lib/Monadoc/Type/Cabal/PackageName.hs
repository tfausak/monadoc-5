module Monadoc.Type.Cabal.PackageName where

import qualified Data.Maybe as Maybe
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Test.Hspec as Hspec

newtype PackageName
  = PackageName Cabal.PackageName
  deriving (Eq, Ord, Show)

instance Sql.ToField PackageName where
  toField = Sql.toField . toString

fromCabal :: Cabal.PackageName -> PackageName
fromCabal = PackageName

fromString :: String -> Maybe PackageName
fromString = fmap fromCabal . Cabal.simpleParsec

toCabal :: PackageName -> Cabal.PackageName
toCabal (PackageName cabal) = cabal

toString :: PackageName -> String
toString = Cabal.prettyShow . toCabal

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Cabal.PackageName" $ do

  Hspec.describe "fromString" $ do

    Hspec.it "works" $ do
      fromString "some-package" `Hspec.shouldSatisfy` Maybe.isJust

  Hspec.describe "toString" $ do

    Hspec.it "works" $ do
      let string = "some-package"
      Just packageName <- pure $ fromString string
      toString packageName `Hspec.shouldBe` string
