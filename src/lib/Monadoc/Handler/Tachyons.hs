module Monadoc.Handler.Tachyons where

import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Network.Wai as Wai
import qualified Test.Hspec as Hspec

handle :: App.App request Wai.Response
handle =
  Common.simpleFileResponse "tachyons-4-12-0.css" "text/css;charset=utf-8"

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Handler.Tachyons" $ do

  Hspec.describe "handle" $ do

    Hspec.it "works" $ do
      Hspec.pending
      -- ctx <- makeContext
      -- response <- App.run
      --   ctx { Context.request = Wai.defaultRequest }
      --   handle
      -- Wai.responseStatus response `Hspec.shouldBe` Http.ok200
