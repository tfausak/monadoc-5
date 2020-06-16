module Monadoc.Type.AppSpec
  ( spec
  )
where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc
import qualified Monadoc.Type.App as App
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.App" $ do

  Test.describe "run" $ do

    Test.it "works" $ do
      let config = Test.config
      context <- Monadoc.configToContext config
      App.run context (pure ()) `Test.shouldReturn` ()

  Test.describe "withConnection" $ do

    Test.it "works" $ do
      let config = Test.config
      context <- Monadoc.configToContext config
      result <- App.run context . App.withConnection $ \connection ->
        IO.liftIO $ Sql.query_ connection "select 1"
      result `Test.shouldBe` [Sql.Only (1 :: Int)]
