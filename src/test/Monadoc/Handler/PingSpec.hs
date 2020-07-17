module Monadoc.Handler.PingSpec where

import qualified Monadoc.Handler.Ping as Ping
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Handler.Ping" $ do

  Test.describe "handle" $ do

    Test.it "works" $ do
      context <- Test.makeContext
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Ping.handle
      Wai.responseStatus response `Test.shouldBe` Http.ok200
