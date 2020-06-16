module Monadoc.Server.RouterSpec
  ( spec
  )
where

import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Route as Route
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Server.Router" $ do

  Test.describe "parseRoute" $ do

    Test.it "works" $ do
      Router.parseRoute "GET" [] `Test.shouldBe` Just Route.Index

  Test.describe "renderAbsoluteRoute" $ do

    Test.it "works" $ do
      let config = Config.initial { Config.url = "http://test" }
      Router.renderAbsoluteRoute config Route.Index
        `Test.shouldBe` "http://test/"

  Test.describe "renderRelativeRoute" $ do

    Test.it "works" $ do
      Router.renderRelativeRoute Route.Index `Test.shouldBe` "/"
