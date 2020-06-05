module Monadoc.Type.WithCallStackSpec
  ( spec
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Maybe as Maybe
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "catch" $ do

    Hspec.it "catches an exception without a call stack" $ do
      WithCallStack.catch
        (Exception.throwM TestException)
        (\ e -> e `Hspec.shouldBe` TestException)

    Hspec.it "catches an exception with a call stack" $ do
      WithCallStack.catch
        (WithCallStack.throw TestException)
        (\ e -> e `Hspec.shouldBe` TestException)

  Hspec.describe "throw" $ do

    Hspec.it "adds a call stack" $ do
      WithCallStack.throw TestException `Hspec.shouldThrow`
        (== Just TestException) . Exception.fromException . WithCallStack.value

  Hspec.describe "withCallStack" $ do

    Hspec.it "adds a call stack" $ do
      let
        x :: Maybe TestException
        x = Monad.join
          . fmap (Exception.fromException . WithCallStack.value)
          . Exception.fromException
          . WithCallStack.withCallStack
          $ Exception.toException TestException
      x `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "does not add two call stacks" $ do
      let
        x :: Maybe TestException
        x = Monad.join
          . fmap (Exception.fromException . WithCallStack.value)
          . Exception.fromException
          . WithCallStack.withCallStack
          . WithCallStack.withCallStack
          $ Exception.toException TestException
      x `Hspec.shouldSatisfy` Maybe.isJust

data TestException
  = TestException
  deriving (Eq, Show)

instance Exception.Exception TestException
