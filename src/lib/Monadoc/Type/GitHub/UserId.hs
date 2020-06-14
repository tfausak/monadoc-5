module Monadoc.Type.GitHub.UserId
  ( UserId
  , fromInt
  , toInt
  )
where

import qualified Data.Aeson as Aeson

-- | A GitHub user's integral ID. This isn't normally surfaced through their
-- UI, but it's a stable identifier. Note that people can change their
-- usernames, which are called "logins" in GitHub parlance.
newtype UserId
  = UserId Int
  deriving (Eq, Show)

instance Aeson.FromJSON UserId where
  parseJSON = fmap fromInt . Aeson.parseJSON

fromInt :: Int -> UserId
fromInt = UserId

toInt :: UserId -> Int
toInt (UserId int) = int
