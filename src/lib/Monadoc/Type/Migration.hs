module Monadoc.Type.Migration where

import qualified Crypto.Hash as Crypto
import qualified Data.Ord as Ord
import qualified Data.Time.Clock.POSIX as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Test.Hspec as Hspec

-- | A database migration. This is a single SQL statement along with a
-- timestamp. The timestamp is used both to order the migrations and as a
-- unique key for identifying them.
data Migration = Migration
  { query :: Sql.Query
  , timestamp :: Timestamp.Timestamp
  } deriving (Eq, Show)

instance Ord Migration where
  compare x y = Ord.comparing timestamp x y <> Ord.comparing query x y

instance Sql.ToRow Migration where
  toRow migration =
    [Sql.toField $ timestamp migration, Sql.toField $ sha256 migration]

-- | Computes a digest of the 'Migration' 'query'. This is used to make sure
-- that the migration hasn't changed since it was ran.
sha256 :: Migration -> Sha256.Sha256
sha256 =
  Sha256.fromDigest . Crypto.hash . Utf8.fromText . Sql.fromQuery . query

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Migration" $ do

  Hspec.describe "sha256" $ do

    Hspec.it "returns the digest of the query" $ do
      let
        migration = Migration
          { query = ""
          , timestamp = Timestamp.fromUtcTime $ Time.posixSecondsToUTCTime 0
          }
      sha256 migration `Hspec.shouldBe` Sha256.fromDigest
        (read
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        )

  Hspec.describe "toRow" $ do

    Hspec.it "converts into a SQL row" $ do
      let
        migration = Migration
          { query = ""
          , timestamp = Timestamp.fromUtcTime $ Time.posixSecondsToUTCTime 0
          }
        row =
          [ Sql.SQLText "1970-01-01T00:00:00.000Z"
          , Sql.SQLText
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          ]
      Sql.toRow migration `Hspec.shouldBe` row
