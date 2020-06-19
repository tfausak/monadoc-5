module Monadoc.Vendor.Sql
  ( Sql.Connection
  , Sql.Field(..)
  , Sql.FromField
  , Sql.FromRow
  , Sql.Ok(..)
  , Sql.Only(..)
  , Sql.Query
  , Sql.SQLData(..)
  , Sql.ToField
  , Sql.ToRow
  , Sql.close
  , Sql.execute
  , Sql.execute_
  , Sql.field
  , Sql.fromField
  , Sql.fromQuery
  , Sql.fromRow
  , Sql.open
  , Sql.query
  , Sql.query_
  , Sql.toField
  , Sql.toRow
  , Sql.withTransaction
  , fromFieldVia
  )
where

import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql

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
