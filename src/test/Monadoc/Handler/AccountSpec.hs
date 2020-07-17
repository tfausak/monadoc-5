module Monadoc.Handler.AccountSpec where

import qualified Monadoc.Handler.Account as Account
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Test

spec :: Spec
spec = describe "Monadoc.Handler.Account" $ do

  describe "handle" $ do

    it "works" $ do
      context <- makeContext
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Account.handle
      Wai.responseStatus response `shouldBe` Http.found302
