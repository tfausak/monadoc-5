module Monadoc.Type.Guid where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.UUID as Uuid
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Utility.Sql as Sql
import qualified System.Random as Random
import qualified Test.Hspec as Hspec

-- | A thin wrapper around a UUID. This is called "GUID" because it's easier to
-- say as a word. It rhymes with "squid".
newtype Guid
  = Guid Uuid.UUID
  deriving (Eq, Show)

instance Sql.FromField Guid where
  fromField = Sql.fromFieldVia $ fmap fromUuid . Uuid.fromText

instance Random.Random Guid where
  random = Bifunctor.first fromUuid . Random.random
  randomR r = Bifunctor.first fromUuid . Random.randomR (both toUuid r)

instance Sql.ToField Guid where
  toField = Sql.toField . Uuid.toText . toUuid

both :: Bifunctor.Bifunctor p => (a -> b) -> p a a -> p b b
both f = Bifunctor.bimap f f

fromUuid :: Uuid.UUID -> Guid
fromUuid = Guid

random :: Random.RandomGen g => g -> (Guid, g)
random = Random.random

toUuid :: Guid -> Uuid.UUID
toUuid (Guid uuid) = uuid

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Guid" $ do

  Hspec.describe "random" $ do

    Hspec.it "generates a random GUID" $ do
      let
        gen = Random.mkStdGen 0
        (guid, _) = random gen
      uuid <- maybe (fail "invalid UUID") pure
        $ Uuid.fromString "fffd04bd-0ede-42e0-8088-a28c5fba9949"
      guid `Hspec.shouldBe` fromUuid uuid
