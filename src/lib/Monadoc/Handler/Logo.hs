module Monadoc.Handler.Logo where

import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Network.Wai as Wai
import qualified Test.Hspec as Hspec

handle :: App.App request Wai.Response
handle = Common.simpleFileResponse "logo.png" "image/png"

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Handler.Logo" $ do

  Hspec.describe "handle" $ do

    Hspec.it "works" $ do
      Hspec.pending
      -- ctx <- makeContext
      -- response <- App.run
      --   ctx { Context.request = Wai.defaultRequest }
      --   handle
      -- Wai.responseStatus response `shouldBe` Http.ok200
