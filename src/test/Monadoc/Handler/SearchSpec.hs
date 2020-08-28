module Monadoc.Handler.SearchSpec where

import qualified Monadoc
import qualified Monadoc.Handler.Search as Search
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Test.Hspec
import Monadoc.Prelude

spec :: Spec
spec = describe "Monadoc.Handler.Search" $ do

  describe "handle" $ do

    it "works" $ do
      ctx <- Monadoc.configToContext Config.test
      response <- App.run
        ctx { Context.request = Wai.defaultRequest }
        Search.handle
      Wai.responseStatus response `shouldBe` Http.ok200
