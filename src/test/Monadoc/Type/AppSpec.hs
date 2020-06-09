module Monadoc.Type.AppSpec
  ( spec
  )
where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.App" $ do

  Hspec.describe "run" $ do

    Hspec.it "works" $ do
      let config = Config.initial { Config.database = ":memory:" }
      context <- Monadoc.configToContext config
      App.run context (pure ()) `Hspec.shouldReturn` ()

  Hspec.describe "withConnection" $ do

    Hspec.it "works" $ do
      let config = Config.initial { Config.database = ":memory:" }
      context <- Monadoc.configToContext config
      result <- App.run context . App.withConnection $ \connection ->
        IO.liftIO . Sql.query_ connection $ Sql.sql "select 1"
      result `Hspec.shouldBe` [Sql.Only (1 :: Int)]
