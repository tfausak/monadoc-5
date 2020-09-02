module Monadoc.Handler.RobotsSpec where

import qualified Monadoc
import qualified Monadoc.Handler.Robots as Robots
import Monadoc.Prelude
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Handler.Robots" <| do

  describe "handle" <| do

    it "works" <| do
      ctx <- Monadoc.configToContext Config.test
      response <- App.run
        ctx { Context.request = Wai.defaultRequest }
        Robots.handle
      Wai.responseStatus response `shouldBe` Http.ok200
