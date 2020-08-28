module Monadoc.Type.WithCallStackSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Maybe as Maybe
import Monadoc.Prelude
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.WithCallStack" <| do

  describe "catch" <| do

    it "catches an exception without a call stack" <| do
      WithCallStack.catch
        (Exception.throwM TestException.TestException)
        (`shouldBe` TestException.TestException)

    it "catches an exception with a call stack" <| do
      WithCallStack.catch
        (WithCallStack.throw TestException.TestException)
        (`shouldBe` TestException.TestException)

  describe "throw" <| do

    it "adds a call stack" <| do
      WithCallStack.throw TestException.TestException
        `shouldThrow` ((== Just TestException.TestException)
                      <<< Exception.fromException
                      <<< WithCallStack.value
                      )

  describe "withCallStack" <| do

    it "adds a call stack" <| do
      let
        x :: Maybe TestException.TestException
        x =
          Monad.join
            <<< map (Exception.fromException <<< WithCallStack.value)
            <<< Exception.fromException
            <<< WithCallStack.withCallStack
            <| Exception.toException TestException.TestException
      x `shouldSatisfy` Maybe.isJust

    it "does not add two call stacks" <| do
      let
        x :: Maybe TestException.TestException
        x =
          Monad.join
            <<< map (Exception.fromException <<< WithCallStack.value)
            <<< Exception.fromException
            <<< WithCallStack.withCallStack
            <<< WithCallStack.withCallStack
            <| Exception.toException TestException.TestException
      x `shouldSatisfy` Maybe.isJust

  -- Testing this is tough because it uses @SomeException@, which doesn't have
  -- an @Eq@ instance. Fortunately this behavior is tested by the @catch@
  -- tests.
  describe "withoutCallStack" <| pure ()
