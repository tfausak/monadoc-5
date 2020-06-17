module Monadoc.Data.VersionSpec
  ( spec
  )
where

import qualified Data.Version as Version
import qualified Monadoc.Data.Version as Monadoc.Version
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Data.Version" $ do

  Test.describe "string" $ do

    Test.it "is not null" $ do
      Monadoc.Version.string `Test.shouldSatisfy` not . null

  Test.describe "version" $ do

    Test.it "has four branches" $ do
      Version.versionBranch Monadoc.Version.version
        `Test.shouldSatisfy` (== 4)
        . length

    Test.it "has no tags" $ do
      let Version.Version _ tags = Monadoc.Version.version
      tags `Test.shouldSatisfy` null
