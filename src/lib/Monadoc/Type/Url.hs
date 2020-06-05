module Monadoc.Type.Url
  ( Url
  , fromUri
  , toUri
  )
where

import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.URI as Uri

-- | A uniform resource locator. Behind the scenes this is a 'Uri.URI'. Use
-- 'fromUri' and 'toUri' to convert into and out of this type. Note that
-- internationalized resource identifiers (IRIs) are not supported.
newtype Url
  = Url Uri.URI
  deriving (Eq, Show)

instance Sql.FromField Url where
  fromField field = do
    string <- Sql.fromField field
    case Uri.parseURI string of
      Nothing ->
        Sql.returnError Sql.ConversionFailed field
          $ "invalid Url: "
          <> show string
      Just uri -> pure $ fromUri uri

instance Sql.ToField Url where
  toField = Sql.toField . ($ "") . Uri.uriToString id . toUri

fromUri :: Uri.URI -> Url
fromUri = Url

toUri :: Url -> Uri.URI
toUri (Url uri) = uri
