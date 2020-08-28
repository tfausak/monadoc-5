module Monadoc.Handler.FaviconSpec where

import qualified Monadoc
import qualified Monadoc.Handler.Favicon as Favicon
import Monadoc.Prelude
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Handler.Favicon" $ do

  describe "handle" $ do

    it "works" $ do
      ctx <- Monadoc.configToContext Config.test
      response <- App.run
        ctx { Context.request = Wai.defaultRequest }
        Favicon.handle
      Wai.responseStatus response `shouldBe` Http.ok200
