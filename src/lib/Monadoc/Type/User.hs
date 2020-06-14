module Monadoc.Type.User
  ( User(..)
  )
where

import qualified Data.Text as Text
import qualified Monadoc.Type.GitHub.Login as Login
import qualified Monadoc.Type.GitHub.UserId as UserId
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Vendor.Sql as Sql

data User = User
  { guid :: Guid.Guid
  , id :: UserId.UserId
  , login :: Login.Login
  , token :: Text.Text
  } deriving (Eq, Show)

instance Sql.FromRow User where
  fromRow = User <$> Sql.field <*> Sql.field <*> Sql.field <*> Sql.field

instance Sql.ToRow User where
  toRow user =
    [ Sql.toField $ guid user
    , Sql.toField $ Monadoc.Type.User.id user
    , Sql.toField $ login user
    , Sql.toField $ token user
    ]