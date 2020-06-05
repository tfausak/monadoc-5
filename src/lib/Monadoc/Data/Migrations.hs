module Monadoc.Data.Migrations
  ( migrations
  )
where

import qualified Data.Fixed as Fixed
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Time as Time

migrations :: Set.Set Migration.Migration
migrations = Set.fromList
  [ makeMigration (2020, 5, 31, 13, 38, 0) "select 1"
  , makeMigration (2020, 6, 2, 13, 43, 0)
    " create table blobs \
    \( octets blob not null \
    \, sha256 text not null primary key \
    \, size integer not null )"
  , makeMigration (2020, 6, 2, 13, 50, 0)
    " create table cache \
    \( etag text not null \
    \, sha256 text not null \
    \, url text not null primary key )"
  ]

makeMigration :: (Integer, Int, Int, Int, Int, Fixed.Pico) -> String -> Migration.Migration
makeMigration (year, month, day, hour, minute, second) query = Migration.Migration
  { Migration.query = Sql.Query $ Text.pack query
  , Migration.timestamp = Timestamp.fromUtcTime $ Time.utcTime year month day hour minute second
  }
