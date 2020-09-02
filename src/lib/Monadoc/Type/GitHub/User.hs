module Monadoc.Type.GitHub.User where

import qualified Data.Aeson as Aeson
import Monadoc.Prelude
import qualified Monadoc.Type.GitHub.Login as Login
import qualified Monadoc.Type.GitHub.UserId as UserId

-- | A GitHub user, as described by their v3 REST API. We only care about a
-- subset of the fields. For a list of all the fields, see
-- <https://developer.github.com/v3/users/#get-a-user>.
data User = User
  { id :: UserId.UserId
  , login :: Login.Login
  } deriving (Eq, Show)

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" <| \object -> do
    id_ <- object Aeson..: "id"
    login_ <- object Aeson..: "login"
    pure User { id = id_, login = login_ }
