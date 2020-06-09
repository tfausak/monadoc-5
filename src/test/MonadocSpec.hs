module MonadocSpec
  ( spec
  )
where

import qualified Monadoc
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.ConfigResult as ConfigResult
import qualified Monadoc.Type.Context as Context
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc" $ do

  Hspec.describe "argumentsToConfigResult" $ do

    Hspec.it "returns the default with no arguments" $ do
      Monadoc.argumentsToConfigResult "x" []
        `Hspec.shouldBe` ConfigResult.Success [] Config.initial

    Hspec.it "shows the help" $ do
      Monadoc.argumentsToConfigResult "x" ["--help"]
        `Hspec.shouldSatisfy` isExitWith

    Hspec.it "shows the version" $ do
      Monadoc.argumentsToConfigResult "x" ["--version"]
        `Hspec.shouldSatisfy` isExitWith

    Hspec.it "fails when given disallowed argument" $ do
      Monadoc.argumentsToConfigResult "x" ["--help=0"]
        `Hspec.shouldSatisfy` isFailure

    Hspec.it "warns when given unexpected parameters" $ do
      case Monadoc.argumentsToConfigResult "x" ["y"] of
        ConfigResult.Success msgs _ -> msgs `Hspec.shouldSatisfy` not . null
        it -> it `Hspec.shouldSatisfy` isSuccess

    Hspec.it "warns when given unknown options" $ do
      case Monadoc.argumentsToConfigResult "x" ["-y"] of
        ConfigResult.Success msgs _ -> msgs `Hspec.shouldSatisfy` not . null
        it -> it `Hspec.shouldSatisfy` isSuccess

    Hspec.it "sets the port" $ do
      case Monadoc.argumentsToConfigResult "x" ["--port=123"] of
        ConfigResult.Success _ config ->
          Config.port config `Hspec.shouldBe` 123
        it -> it `Hspec.shouldSatisfy` isSuccess

  Hspec.describe "configToContext" $ do

    Hspec.it "works" $ do
      let config = Config.initial { Config.database = ":memory:" }
      context <- Monadoc.configToContext config
      Context.config context `Hspec.shouldBe` config

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
