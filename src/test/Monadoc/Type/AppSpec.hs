module Monadoc.Type.AppSpec
  ( spec
  )
where

import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "run" $ do

    Hspec.it "works" $ do
      let config = Config.initial { Config.database = ":memory:" }
      context <- Context.fromConfig config
      App.run context (pure ()) `Hspec.shouldReturn` ()
