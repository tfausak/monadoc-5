module Monadoc.Handler.IndexSpec where

import qualified Monadoc.Handler.Index as Index
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Test

spec :: Spec
spec = describe "Monadoc.Handler.Index" $ do

  describe "handle" $ do

    it "works" $ do
      context <- makeContext
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Index.handle
      Wai.responseStatus response `shouldBe` Http.ok200
