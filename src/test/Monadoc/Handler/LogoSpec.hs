module Monadoc.Handler.LogoSpec where

import qualified Monadoc.Handler.Logo as Logo
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Test

spec :: Spec
spec = describe "Monadoc.Handler.Logo" $ do

  describe "handle" $ do

    it "works" $ do
      context <- makeContext
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Logo.handle
      Wai.responseStatus response `shouldBe` Http.ok200
