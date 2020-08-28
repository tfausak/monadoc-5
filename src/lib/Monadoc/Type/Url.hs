module Monadoc.Type.Url where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Utility.Sql as Sql
import qualified Network.URI as Uri
import Monadoc.Prelude

-- | A uniform resource locator. Behind the scenes this is a 'Uri.URI'. Use
-- 'fromUri' and 'toUri' to convert into and out of this type. Note that
-- internationalized resource identifiers (IRIs) are not supported.
newtype Url
  = Url Uri.URI
  deriving (Eq, Show)

instance Sql.FromField Url where
  fromField = Sql.fromFieldVia $ fmap fromUri . Uri.parseURI

instance Sql.ToField Url where
  toField = Sql.toField . ($ "") . Uri.uriToString id . toUri

fromUri :: Uri.URI -> Url
fromUri = Url

toUri :: Url -> Uri.URI
toUri (Url uri) = uri
