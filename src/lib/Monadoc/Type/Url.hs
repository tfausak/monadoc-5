module Monadoc.Type.Url where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude
import qualified Monadoc.Utility.Sql as Sql
import qualified Network.URI as Uri

-- | A uniform resource locator. Behind the scenes this is a 'Uri.URI'. Use
-- 'fromUri' and 'toUri' to convert into and out of this type. Note that
-- internationalized resource identifiers (IRIs) are not supported.
newtype Url
  = Url Uri.URI
  deriving (Eq, Show)

instance Sql.FromField Url where
  fromField = Sql.fromFieldVia <| map fromUri <<< Uri.parseURI

instance Sql.ToField Url where
  toField = Sql.toField <<< (<| "") <<< Uri.uriToString identity <<< toUri

fromUri :: Uri.URI -> Url
fromUri = Url

toUri :: Url -> Uri.URI
toUri (Url uri) = uri
