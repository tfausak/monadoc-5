module Monadoc.Data.Migrations
  ( migrations
  )
where

import qualified Data.Fixed as Fixed
import qualified Data.Set as Set
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Vendor.Time as Time

-- | Collection of migrations to run. The app automatically performs migrations
-- when it starts. They are run ordered by their timestamps.
--
-- Because an old version of the app may be running when a new version
-- launches, migrations needs to be compatible. Both old and new code must be
-- able to deal with both old and new databases.
migrations :: Set.Set Migration.Migration
migrations = Set.fromList
  [ makeMigration (2020, 5, 31, 13, 38, 0) "select 1"
  , makeMigration
    (2020, 6, 2, 13, 43, 0)
    " create table blobs \
    \( octets blob not null \
    \, sha256 text not null primary key \
    \, size integer not null )"
  , makeMigration
    (2020, 6, 2, 13, 50, 0)
    " create table cache \
    \( etag text not null \
    \, sha256 text not null \
    \, url text not null primary key )"
  ]

makeMigration
  :: (Integer, Int, Int, Int, Int, Fixed.Pico) -> String -> Migration.Migration
makeMigration (year, month, day, hour, minute, second) query =
  Migration.Migration
    { Migration.query = Sql.sql query
    , Migration.timestamp = Timestamp.fromUtcTime
      $ Time.utcTime year month day hour minute second
    }
