module Monadoc.Type.Path
  ( Path
  , fromFilePath
  , fromStrings
  , toFilePath
  , toStrings
  )
where

import qualified Monadoc.Vendor.Sql as Sql
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows

-- | A relative file path. Typically these come from tar entries. We store each
-- path segment separately to avoid directory separator problems between Linux
-- and Windows.
newtype Path
  = Path [String]
  deriving (Eq, Show)

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
