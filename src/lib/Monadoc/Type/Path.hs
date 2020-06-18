module Monadoc.Type.Path
  ( Path
  , fromFilePath
  , toFilePath
  )
where

import qualified Data.List as List
import qualified Monadoc.Vendor.Sql as Sql
import qualified System.FilePath.Windows as FilePath

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
fromFilePath = Path . FilePath.splitDirectories

-- | Converts to a file path by joining with @/@.
toFilePath :: Path -> String
toFilePath (Path strings) = List.intercalate "/" strings
