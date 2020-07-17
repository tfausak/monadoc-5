module MonadocSpec where

import qualified Monadoc
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.ConfigResult as ConfigResult
import qualified Monadoc.Type.Context as Context
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc" $ do

  Test.describe "argumentsToConfigResult" $ do

    Test.it "returns the default with no arguments" $ do
      Monadoc.argumentsToConfigResult "x" []
        `Test.shouldBe` ConfigResult.Success [] Config.initial

    Test.it "shows the help" $ do
      Monadoc.argumentsToConfigResult "x" ["--help"]
        `Test.shouldSatisfy` isExitWith

    Test.it "shows the version" $ do
      Monadoc.argumentsToConfigResult "x" ["--version"]
        `Test.shouldSatisfy` isExitWith

    Test.it "fails when given disallowed argument" $ do
      Monadoc.argumentsToConfigResult "x" ["--help=0"]
        `Test.shouldSatisfy` isFailure

    Test.it "warns when given unexpected parameters" $ do
      case Monadoc.argumentsToConfigResult "x" ["y"] of
        ConfigResult.Success msgs _ -> msgs `Test.shouldSatisfy` not . null
        it -> it `Test.shouldSatisfy` isSuccess

    Test.it "warns when given unknown options" $ do
      case Monadoc.argumentsToConfigResult "x" ["-y"] of
        ConfigResult.Success msgs _ -> msgs `Test.shouldSatisfy` not . null
        it -> it `Test.shouldSatisfy` isSuccess

    Test.it "sets the port" $ do
      case Monadoc.argumentsToConfigResult "x" ["--port=123"] of
        ConfigResult.Success _ config ->
          Config.port config `Test.shouldBe` 123
        it -> it `Test.shouldSatisfy` isSuccess

  Test.describe "configToContext" $ do

    Test.it "works" $ do
      let config = Test.config
      context <- Monadoc.configToContext config
      Context.config context `Test.shouldBe` config

isExitWith :: ConfigResult.ConfigResult -> Bool
isExitWith configResult = case configResult of
  ConfigResult.ExitWith _ -> True
  _ -> False

isFailure :: ConfigResult.ConfigResult -> Bool
isFailure configResult = case configResult of
  ConfigResult.Failure _ -> True
  _ -> False

isSuccess :: ConfigResult.ConfigResult -> Bool
isSuccess configResult = case configResult of
  ConfigResult.Success _ _ -> True
  _ -> False
