module Monadoc.Type.AppSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Type.App as App
import qualified Monadoc.Vendor.Sql as Sql
import Test

spec :: Spec
spec = describe "Monadoc.Type.App" $ do

  describe "run" $ do

    it "works" $ do
      ctx <- makeContext
      App.run ctx (pure ()) `shouldReturn` ()

  describe "withConnection" $ do

    it "works" $ do
      ctx <- makeContext
      result <- App.run ctx . App.withConnection $ \connection ->
        IO.liftIO $ Sql.query_ connection "select 1"
      result `shouldBe` [Sql.Only (1 :: Int)]
