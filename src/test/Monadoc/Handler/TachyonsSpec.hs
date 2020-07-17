module Monadoc.Handler.TachyonsSpec where

import qualified Monadoc.Handler.Tachyons as Tachyons
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Handler.Tachyons" $ do

  Test.describe "handle" $ do

    Test.it "works" $ do
      context <- Test.makeContext
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Tachyons.handle
      Wai.responseStatus response `Test.shouldBe` Http.ok200
