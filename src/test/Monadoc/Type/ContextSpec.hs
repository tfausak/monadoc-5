module Monadoc.Type.ContextSpec
  ( spec
  )
where

import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "fromConfig" $ do

    Hspec.it "works" $ do
      let config = Config.initial { Config.database = ":memory:" }
      context <- Context.fromConfig config
      Context.config context `Hspec.shouldBe` config
