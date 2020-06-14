module Monadoc.Type.GitHub.Login
  ( Login
  , fromText
  , toText
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Monadoc.Vendor.Sql as Sql

-- | A GitHub user's login, which is more commonly known as their username.
-- This is the part that comes after \@, like \@tfausak.
newtype Login
  = Login Text.Text
  deriving (Eq, Show)

instance Sql.FromField Login where
  fromField = fmap fromText . Sql.fromField

instance Aeson.FromJSON Login where
  parseJSON = fmap fromText . Aeson.parseJSON

instance Sql.ToField Login where
  toField = Sql.toField . toText

fromText :: Text.Text -> Login
fromText = Login

toText :: Login -> Text.Text
toText (Login text) = text
