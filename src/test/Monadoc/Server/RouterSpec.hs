module Monadoc.Server.RouterSpec
  ( spec
  )
where

import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Route as Route
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Server.Router" $ do

  Hspec.describe "parseRoute" $ do

    Hspec.it "works" $ do
      Router.parseRoute "GET" [] `Hspec.shouldBe` Just Route.Index

  Hspec.describe "renderAbsoluteRoute" $ do

    Hspec.it "works" $ do
      let config = Config.initial { Config.url = "http://test" }
      Router.renderAbsoluteRoute config Route.Index
        `Hspec.shouldBe` "http://test/"

  Hspec.describe "renderRelativeRoute" $ do

    Hspec.it "works" $ do
      Router.renderRelativeRoute Route.Index `Hspec.shouldBe` "/"
