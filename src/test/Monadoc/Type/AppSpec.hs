module Monadoc.Type.AppSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc
import Monadoc.Prelude
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.App" <| do

  describe "run" <| do

    it "works" <| do
      ctx <- Monadoc.configToContext Config.test
      App.run ctx (pure ()) `shouldReturn` ()

  describe "withConnection" <| do

    it "works" <| do
      ctx <- Monadoc.configToContext Config.test
      result <- App.run ctx <<< App.withConnection <| \connection ->
        IO.liftIO <| Sql.query_ connection "select 1"
      result `shouldBe` [Sql.Only (1 :: Int)]
