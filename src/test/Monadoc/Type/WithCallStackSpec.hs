module Monadoc.Type.WithCallStackSpec
  ( spec
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Maybe as Maybe
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.WithCallStack" $ do

  Test.describe "catch" $ do

    Test.it "catches an exception without a call stack" $ do
      WithCallStack.catch
        (Exception.throwM TestException.TestException)
        (`Test.shouldBe` TestException.TestException)

    Test.it "catches an exception with a call stack" $ do
      WithCallStack.catch
        (WithCallStack.throw TestException.TestException)
        (`Test.shouldBe` TestException.TestException)

  Test.describe "throw" $ do

    Test.it "adds a call stack" $ do
      WithCallStack.throw TestException.TestException
        `Test.shouldThrow` (== Just TestException.TestException)
        . Exception.fromException
        . WithCallStack.value

  Test.describe "withCallStack" $ do

    Test.it "adds a call stack" $ do
      let
        x :: Maybe TestException.TestException
        x =
          Monad.join
            . fmap (Exception.fromException . WithCallStack.value)
            . Exception.fromException
            . WithCallStack.withCallStack
            $ Exception.toException TestException.TestException
      x `Test.shouldSatisfy` Maybe.isJust

    Test.it "does not add two call stacks" $ do
      let
        x :: Maybe TestException.TestException
        x =
          Monad.join
            . fmap (Exception.fromException . WithCallStack.value)
            . Exception.fromException
            . WithCallStack.withCallStack
            . WithCallStack.withCallStack
            $ Exception.toException TestException.TestException
      x `Test.shouldSatisfy` Maybe.isJust

  -- Testing this is tough because it uses @SomeException@, which doesn't have
  -- an @Eq@ instance. Fortunately this behavior is tested by the @catch@
  -- tests.
  Test.describe "withoutCallStack" $ pure ()
