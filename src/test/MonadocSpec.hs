module MonadocSpec where

import qualified Monadoc
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.ConfigResult as ConfigResult
import qualified Monadoc.Type.Context as Context
import Test

spec :: Spec
spec = describe "Monadoc" $ do

  describe "argumentsToConfigResult" $ do

    it "returns the default with no arguments" $ do
      Monadoc.argumentsToConfigResult "x" []
        `shouldBe` ConfigResult.Success [] Config.initial

    it "shows the help" $ do
      Monadoc.argumentsToConfigResult "x" ["--help"] `shouldSatisfy` isExitWith

    it "shows the version" $ do
      Monadoc.argumentsToConfigResult "x" ["--version"]
        `shouldSatisfy` isExitWith

    it "fails when given disallowed argument" $ do
      Monadoc.argumentsToConfigResult "x" ["--help=0"]
        `shouldSatisfy` isFailure

    it "warns when given unexpected parameters" $ do
      case Monadoc.argumentsToConfigResult "x" ["y"] of
        ConfigResult.Success msgs _ -> msgs `shouldSatisfy` not . null
        result -> result `shouldSatisfy` isSuccess

    it "warns when given unknown options" $ do
      case Monadoc.argumentsToConfigResult "x" ["-y"] of
        ConfigResult.Success msgs _ -> msgs `shouldSatisfy` not . null
        result -> result `shouldSatisfy` isSuccess

    it "sets the port" $ do
      case Monadoc.argumentsToConfigResult "x" ["--port=123"] of
        ConfigResult.Success _ cfg -> Config.port cfg `shouldBe` 123
        result -> result `shouldSatisfy` isSuccess

  describe "configToContext" $ do

    it "works" $ do
      ctx <- Monadoc.configToContext Config.test
      Context.config ctx `shouldBe` Config.test

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
