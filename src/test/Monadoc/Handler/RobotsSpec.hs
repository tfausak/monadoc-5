module Monadoc.Handler.RobotsSpec where

import qualified Monadoc.Handler.Robots as Robots
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Handler.Robots" $ do

  Test.describe "handle" $ do

    Test.it "works" $ do
      context <- Test.makeContext
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Robots.handle
      Wai.responseStatus response `Test.shouldBe` Http.ok200
