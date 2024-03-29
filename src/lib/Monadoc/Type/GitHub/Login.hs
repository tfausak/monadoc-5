module Monadoc.Type.GitHub.Login where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude

-- | A GitHub user's login, which is more commonly known as their username.
-- This is the part that comes after \@, like \@tfausak.
newtype Login
  = Login Text.Text
  deriving (Eq, Show)

instance Sql.FromField Login where
  fromField = map fromText <<< Sql.fromField

instance Aeson.FromJSON Login where
  parseJSON = map fromText <<< Aeson.parseJSON

instance Sql.ToField Login where
  toField = Sql.toField <<< toText

fromText :: Text.Text -> Login
fromText = Login

toText :: Login -> Text.Text
toText (Login text) = text
