module Monadoc.Server.RouterSpec where

import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Route as Route
import Test

spec :: Spec
spec = describe "Monadoc.Server.Router" $ do

  describe "parseRoute" $ do

    it "works" $ do
      Router.parseRoute "GET" [] `shouldBe` Just Route.Index

  describe "renderAbsoluteRoute" $ do

    it "works" $ do
      let config = testConfig { Config.url = "http://test" }
      Router.renderAbsoluteRoute config Route.Index `shouldBe` "http://test/"

  describe "renderRelativeRoute" $ do

    it "works" $ do
      Router.renderRelativeRoute Route.Index `shouldBe` "/"
