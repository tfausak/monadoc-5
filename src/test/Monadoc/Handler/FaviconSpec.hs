module Monadoc.Handler.FaviconSpec where

import qualified Monadoc.Handler.Favicon as Favicon
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Test

spec :: Spec
spec = describe "Monadoc.Handler.Favicon" $ do

  describe "handle" $ do

    it "works" $ do
      context <- makeContext
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Favicon.handle
      Wai.responseStatus response `shouldBe` Http.ok200
