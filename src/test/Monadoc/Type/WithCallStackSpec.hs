module Monadoc.Type.WithCallStackSpec
  ( spec
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Maybe as Maybe
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.WithCallStack" $ do

  Hspec.describe "catch" $ do

    Hspec.it "catches an exception without a call stack" $ do
      WithCallStack.catch
        (Exception.throwM TestException.TestException)
        (`Hspec.shouldBe` TestException.TestException)

    Hspec.it "catches an exception with a call stack" $ do
      WithCallStack.catch
        (WithCallStack.throw TestException.TestException)
        (`Hspec.shouldBe` TestException.TestException)

  Hspec.describe "throw" $ do

    Hspec.it "adds a call stack" $ do
      WithCallStack.throw TestException.TestException
        `Hspec.shouldThrow` (== Just TestException.TestException)
        . Exception.fromException
        . WithCallStack.value

  Hspec.describe "withCallStack" $ do

    Hspec.it "adds a call stack" $ do
      let
        x :: Maybe TestException.TestException
        x =
          Monad.join
            . fmap (Exception.fromException . WithCallStack.value)
            . Exception.fromException
            . WithCallStack.withCallStack
            $ Exception.toException TestException.TestException
      x `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "does not add two call stacks" $ do
      let
        x :: Maybe TestException.TestException
        x =
          Monad.join
            . fmap (Exception.fromException . WithCallStack.value)
            . Exception.fromException
            . WithCallStack.withCallStack
            . WithCallStack.withCallStack
            $ Exception.toException TestException.TestException
      x `Hspec.shouldSatisfy` Maybe.isJust

  -- Testing this is tough because it uses @SomeException@, which doesn't have
  -- an @Eq@ instance. Fortunately this behavior is tested by the @catch@
  -- tests.
  Hspec.describe "withoutCallStack" $ pure ()
