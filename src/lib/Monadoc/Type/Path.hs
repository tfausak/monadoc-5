module Monadoc.Type.Path where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import qualified Test.Hspec as Hspec

-- | A relative file path. Typically these come from tar entries. We store each
-- path segment separately to avoid directory separator problems between Linux
-- and Windows.
newtype Path
  = Path [String]
  deriving (Eq, Show)

instance Monoid Path where
  mempty = fromStrings mempty

instance Sql.FromField Path where
  fromField = fmap fromFilePath . Sql.fromField

instance Semigroup Path where
  x <> y = fromStrings $ toStrings x <> toStrings y

instance Sql.ToField Path where
  toField = Sql.toField . toFilePath

-- | Converts from a file path by splitting on both @/@ and @\\@.
fromFilePath :: FilePath -> Path
fromFilePath = fromStrings . Windows.splitDirectories

fromStrings :: [String] -> Path
fromStrings = Path

-- | Converts to a file path by joining with @/@.
toFilePath :: Path -> String
toFilePath = Posix.joinPath . toStrings

toStrings :: Path -> [String]
toStrings (Path strings) = strings

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Path" $ do

  Hspec.describe "fromFilePath" $ do

    Hspec.it "treats forward and backward slashes the same" $ do
      fromFilePath "a/b" `Hspec.shouldBe` fromFilePath "a\\b"

  Hspec.describe "toFilePath" $ do

    Hspec.it "uses forward slashes" $ do
      toFilePath (fromFilePath "a\\b") `Hspec.shouldBe` "a/b"
