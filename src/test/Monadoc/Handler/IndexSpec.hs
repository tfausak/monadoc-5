module Monadoc.Handler.IndexSpec
  ( spec
  )
where

import qualified Monadoc
import qualified Monadoc.Handler.Index as Index
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Handler.Index" $ do

  Hspec.describe "handle" $ do

    Hspec.it "works" $ do
      context <- Monadoc.configToContext Config.initial
        { Config.database = ":memory:"
        }
      response <- App.run
        context { Context.request = Wai.defaultRequest }
        Index.handle
      Wai.responseStatus response `Hspec.shouldBe` Http.ok200
