module Monadoc.Vendor.Sqlite
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

-- | Converts a string into a SQL query. This is purely for convenience when
-- avoiding @OverloadedStrings@.
sql :: String -> Database.SQLite.Simple.Query
sql = Database.SQLite.Simple.Query . Text.pack
