module Monadoc.Handler.ThrowSpec
  ( spec
  )
where

import qualified Monadoc
import qualified Monadoc.Handler.Throw as Throw
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai as Wai
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Handler.Throw" $ do

  Hspec.describe "handle" $ do

    Hspec.it "works" $ do
      context <- Monadoc.configToContext Config.initial
        { Config.database = ":memory:"
        }
      let
        it =
          App.run context { Context.request = Wai.defaultRequest } Throw.handle
      it `Hspec.shouldThrow` Hspec.anyException
