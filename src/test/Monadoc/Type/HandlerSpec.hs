module Monadoc.Type.HandlerSpec
  ( spec
  )
where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.Wai as Wai
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Handler" $ do

  Hspec.describe "run" $ do

    Hspec.it "works" $ do
      let config = Config.initial { Config.database = ":memory:" }
      context <- Monadoc.configToContext config
      Handler.run context Wai.defaultRequest (pure ()) `Hspec.shouldReturn` ()

  Hspec.describe "withConnection" $ do

    Hspec.it "works" $ do
      let config = Config.initial { Config.database = ":memory:" }
      context <- Monadoc.configToContext config
      result <-
        Handler.run context Wai.defaultRequest
        . Handler.withConnection
        $ \connection -> IO.liftIO $ Sql.query_ connection "select 1"
      result `Hspec.shouldBe` [Sql.Only (1 :: Int)]
