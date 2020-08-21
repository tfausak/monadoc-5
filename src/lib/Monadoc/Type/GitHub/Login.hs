module Monadoc.Type.GitHub.Login where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

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

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.GitHub.Login" $ do

  Hspec.it "needs tests" Hspec.pending
