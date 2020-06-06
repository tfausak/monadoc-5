module Monadoc.Vendor.Sql
  ( Database.SQLite.Simple.Connection
  , Database.SQLite.Simple.FromField.FromField(..)
  , Database.SQLite.Simple.FromField.ResultError(..)
  , Database.SQLite.Simple.Internal.Field(..)
  , Database.SQLite.Simple.Ok.Ok(..)
  , Database.SQLite.Simple.Only(..)
  , Database.SQLite.Simple.Query(..)
  , Database.SQLite.Simple.SQLData(..)
  , Database.SQLite.Simple.ToField.ToField(..)
  , Database.SQLite.Simple.ToRow(..)
  , Database.SQLite.Simple.close
  , Database.SQLite.Simple.execute
  , Database.SQLite.Simple.execute_
  , fromFieldVia
  , Database.SQLite.Simple.open
  , Database.SQLite.Simple.query
  , Database.SQLite.Simple.query_
  , Database.SQLite.Simple.FromField.returnError
  , sql
  , Database.SQLite.Simple.withTransaction
  )
where

import qualified Database.SQLite.Simple
import qualified Database.SQLite.Simple.FromField
import qualified Database.SQLite.Simple.Internal
import qualified Database.SQLite.Simple.Ok
import qualified Database.SQLite.Simple.ToField
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable

-- | Converts from a SQL field into a value using the given function. This is
-- mostly used to avoid all the boilerplate.
fromFieldVia
  :: ( Database.SQLite.Simple.FromField.FromField a
     , Show a
     , Typeable.Typeable b
     )
  => (a -> Maybe b)
  -> Database.SQLite.Simple.FromField.Field
  -> Database.SQLite.Simple.Ok.Ok b
fromFieldVia f field = do
  x <- Database.SQLite.Simple.FromField.fromField field
  case f x of
    Nothing ->
      Database.SQLite.Simple.FromField.returnError
          Database.SQLite.Simple.ConversionFailed
          field
        $ "failed to convert "
        <> show x
    Just y -> pure y

-- | Converts a string into a SQL query. This is purely for convenience when
-- avoiding @OverloadedStrings@.
sql :: String -> Database.SQLite.Simple.Query
sql = Database.SQLite.Simple.Query . Text.pack
