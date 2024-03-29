module Monadoc.Type.Path where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows

-- | A relative file path. Typically these come from tar entries. We store each
-- path segment separately to avoid directory separator problems between Linux
-- and Windows.
newtype Path
  = Path [String]
  deriving (Eq, Show)

instance Monoid Path where
  mempty = fromStrings mempty

instance Sql.FromField Path where
  fromField = map fromFilePath <<< Sql.fromField

instance Semigroup Path where
  x <> y = fromStrings <| toStrings x <> toStrings y

instance Sql.ToField Path where
  toField = Sql.toField <<< toFilePath

-- | Converts from a file path by splitting on both @/@ and @\\@.
fromFilePath :: FilePath -> Path
fromFilePath = fromStrings <<< Windows.splitDirectories

fromStrings :: [String] -> Path
fromStrings = Path

-- | Converts to a file path by joining with @/@.
toFilePath :: Path -> String
toFilePath = Posix.joinPath <<< toStrings

toStrings :: Path -> [String]
toStrings (Path strings) = strings
