module Monadoc.Vendor.Sql
  ( module Database.SQLite.Simple
  , module Database.SQLite.Simple.FromField
  , module Database.SQLite.Simple.Internal
  , module Database.SQLite.Simple.Ok
  , module Database.SQLite.Simple.ToField
  , fromFieldVia
  , sql
  )
where

import Database.SQLite.Simple
  ( Connection
  , Only(Only, fromOnly)
  , Query(Query, fromQuery)
  , SQLData(SQLBlob, SQLInteger, SQLFloat, SQLNull, SQLText)
  , ToRow(toRow)
  , close
  , execute
  , execute_
  , open
  , query
  , query_
  , withTransaction
  )
import Database.SQLite.Simple.FromField
  ( FromField(fromField)
  , ResultError(ConversionFailed, Incompatible, UnexpectedNull)
  , returnError
  )
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Errors, Ok))
import Database.SQLite.Simple.ToField (ToField(toField))

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable

-- | Converts from a SQL field into a value using the given function. This is
-- mostly used to avoid all the boilerplate.
fromFieldVia
  :: (Sql.FromField a, Show a, Typeable.Typeable b)
  => (a -> Maybe b)
  -> Sql.Field
  -> Sql.Ok b
fromFieldVia f field = do
  x <- Sql.fromField field
  case f x of
    Nothing ->
      Sql.returnError Sql.ConversionFailed field
        $ "failed to convert "
        <> show x
    Just y -> pure y

-- | Converts a string into a SQL query. This is purely for convenience when
-- avoiding @OverloadedStrings@.
sql :: String -> Sql.Query
sql = Sql.Query . Text.pack
