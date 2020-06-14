module Monadoc.Vendor.Sql
  ( module Database.SQLite.Simple
  , module Database.SQLite.Simple.FromField
  , module Database.SQLite.Simple.Internal
  , module Database.SQLite.Simple.Ok
  , module Database.SQLite.Simple.ToField
  , fromFieldVia
  )
where

import Database.SQLite.Simple
  ( Connection
  , FromRow(fromRow)
  , Only(Only, fromOnly)
  , Query(Query, fromQuery)
  , SQLData(SQLBlob, SQLFloat, SQLInteger, SQLNull, SQLText)
  , ToRow(toRow)
  , close
  , execute
  , execute_
  , field
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

import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Ok as Sql

-- | Converts from a SQL field into a value using the given function. This is
-- mostly used to avoid all the boilerplate.
fromFieldVia
  :: (Sql.FromField a, Show a, Typeable.Typeable b)
  => (a -> Maybe b)
  -> Sql.Field
  -> Sql.Ok b
fromFieldVia f x = do
  y <- Sql.fromField x
  case f y of
    Nothing ->
      Sql.returnError Sql.ConversionFailed x $ "failed to convert " <> show y
    Just z -> pure z
