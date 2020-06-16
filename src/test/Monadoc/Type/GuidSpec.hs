module Monadoc.Type.GuidSpec
  ( spec
  )
where

import qualified Data.UUID as Uuid
import qualified Monadoc.Type.Guid as Guid
import qualified System.Random as Random
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Guid" $ do

  Test.describe "random" $ do

    Test.it "generates a random GUID" $ do
      let
        gen = Random.mkStdGen 0
        (guid, _) = Guid.random gen
      uuid <- maybe (fail "invalid UUID") pure
        $ Uuid.fromString "fffd04bd-0ede-42e0-8088-a28c5fba9949"
      guid `Test.shouldBe` Guid.fromUuid uuid
