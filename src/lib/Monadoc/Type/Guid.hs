module Monadoc.Type.Guid
  ( Guid
  , fromUuid
  , random
  , toUuid
  )
where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.UUID as Uuid
import qualified System.Random as Random

-- | A thin wrapper around a UUID. This is called "GUID" because it's easier to
-- say as a word. It rhymes with "squid".
newtype Guid
  = Guid Uuid.UUID
  deriving (Eq, Show)

instance Random.Random Guid where
  random = Bifunctor.first fromUuid . Random.random
  randomR r = Bifunctor.first fromUuid . Random.randomR (both toUuid r)

both :: Bifunctor.Bifunctor p => (a -> b) -> p a a -> p b b
both f = Bifunctor.bimap f f

fromUuid :: Uuid.UUID -> Guid
fromUuid = Guid

random :: Random.RandomGen g => g -> (Guid, g)
random = Random.random

toUuid :: Guid -> Uuid.UUID
toUuid (Guid uuid) = uuid
