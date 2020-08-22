module Monadoc.Type.Revision where

import qualified Database.SQLite.Simple.ToField as Sql
import qualified Test.Hspec as Hspec
import qualified Text.Read as Read

newtype Revision
  = Revision Word
  deriving (Eq, Show)

instance Sql.ToField Revision where
  toField = Sql.toField . toWord

fromString :: String -> Maybe Revision
fromString = fmap Revision . Read.readMaybe

fromWord :: Word -> Revision
fromWord = Revision

increment :: Revision -> Revision
increment = fromWord . (+ 1) . toWord

toString :: Revision -> String
toString = show . toWord

toWord :: Revision -> Word
toWord (Revision word) = word

zero :: Revision
zero = fromWord 0

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Revision" $ do

  Hspec.describe "increment" $ do

    Hspec.it "increases by one" $ do
      increment zero `Hspec.shouldBe` fromWord 1

  Hspec.describe "toString" $ do

    Hspec.it "renders just the number" $ do
      toString zero `Hspec.shouldBe` "0"
