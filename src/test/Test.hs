module Test
  ( Hspec.Spec
  , Hspec.describe
  , Hspec.hspec
  , Hspec.it
  , Hspec.pending
  , Hspec.shouldBe
  , Hspec.shouldReturn
  , Hspec.shouldSatisfy
  , Hspec.shouldThrow
  , testConfig
  , makeContext
  )
where

import qualified Monadoc
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Test.Hspec as Hspec

testConfig :: Config.Config
testConfig =
  Config.initial { Config.database = ":memory:", Config.url = "http://test" }

makeContext :: IO (Context.Context ())
makeContext = Monadoc.configToContext testConfig
