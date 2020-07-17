module Monadoc.Handler.LogoSpec where

import qualified Monadoc.Handler.Logo as Logo
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Handler.Logo" $ do

  Test.describe "handle" $ do

    Test.it "works" $ do
      context <- Test.makeContext
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Logo.handle
      Wai.responseStatus response `Test.shouldBe` Http.ok200
