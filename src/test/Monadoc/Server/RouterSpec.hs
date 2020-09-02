module Monadoc.Server.RouterSpec where

import Monadoc.Prelude
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Route as Route
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Server.Router" <| do

  describe "parseRoute" <| do

    it "works" <| do
      Router.parseRoute "GET" [] `shouldBe` Just Route.Index

  describe "renderAbsoluteRoute" <| do

    it "works" <| do
      Router.renderAbsoluteRoute Config.test Route.Index
        `shouldBe` "http://monadoc.test:4444/"

  describe "renderRelativeRoute" <| do

    it "works" <| do
      Router.renderRelativeRoute Route.Index `shouldBe` "/"
