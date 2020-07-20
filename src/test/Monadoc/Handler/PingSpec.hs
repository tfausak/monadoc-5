module Monadoc.Handler.PingSpec where

import qualified Monadoc.Handler.Ping as Ping
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Test

spec :: Spec
spec = describe "Monadoc.Handler.Ping" $ do

  describe "handle" $ do

    it "works" $ do
      ctx <- makeContext
      response <- App.run
        ctx { Context.request = Wai.defaultRequest }
        Ping.handle
      Wai.responseStatus response `shouldBe` Http.ok200
