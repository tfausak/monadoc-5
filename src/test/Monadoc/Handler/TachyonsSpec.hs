module Monadoc.Handler.TachyonsSpec
  ( spec
  )
where

import qualified Monadoc
import qualified Monadoc.Handler.Tachyons as Tachyons
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Handler.Tachyons" $ do

  Test.describe "handle" $ do

    Test.it "works" $ do
      context <- Monadoc.configToContext Config.initial
        { Config.database = ":memory:"
        }
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Tachyons.handle
      Wai.responseStatus response `Test.shouldBe` Http.ok200
