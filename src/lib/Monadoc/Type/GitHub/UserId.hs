module Monadoc.Type.GitHub.UserId
  ( UserId
  , fromInt
  , toInt
  )
where

import qualified Data.Aeson as Aeson
import qualified Monadoc.Vendor.Sql as Sql

-- | A GitHub user's integral ID. This isn't normally surfaced through their
-- UI, but it's a stable identifier. Note that people can change their
-- usernames, which are called "logins" in GitHub parlance.
newtype UserId
  = UserId Int
  deriving (Eq, Show)

instance Sql.FromField UserId where
  fromField = fmap fromInt . Sql.fromField

instance Aeson.FromJSON UserId where
  parseJSON = fmap fromInt . Aeson.parseJSON

instance Sql.ToField UserId where
  toField = Sql.toField . toInt

fromInt :: Int -> UserId
fromInt = UserId

toInt :: UserId -> Int
toInt (UserId int) = int
