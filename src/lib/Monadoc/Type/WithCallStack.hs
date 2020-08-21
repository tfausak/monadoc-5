module Monadoc.Type.WithCallStack where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Function as Function
import qualified Data.Maybe as Maybe
import qualified GHC.Stack as Stack
import qualified Monadoc.Type.TestException as TestException
import qualified Test.Hspec as Hspec

-- | Some value with a 'Stack.CallStack' attached. Typically this is used with
-- 'Exception.SomeException' to attach call stacks to exceptions.
data WithCallStack a = WithCallStack
  { callStack :: Stack.CallStack
  , value :: a
  } deriving Show

instance Eq a => Eq (WithCallStack a) where
  x == y =
    Function.on (==) (Stack.getCallStack . callStack) x y
      && Function.on (==) value x y

instance Exception.Exception e => Exception.Exception (WithCallStack e) where
  displayException x =
    let string = Exception.displayException $ value x
    in
      case Stack.prettyCallStack $ callStack x of
        "" -> string
        stack -> mconcat [string, "\n", stack]

-- | Catches an exception, removing call stacks as necessary. This wraps
-- 'withoutCallStack' to make it easy to catch an exception without a call
-- stack even if it was thrown with one. You should prefer this over
-- 'Exception.catch' when possible.
catch
  :: (Exception.MonadCatch m, Exception.Exception e)
  => m a
  -> (e -> m a)
  -> m a
catch x f = Exception.catches
  x
  [ Exception.Handler f
  , Exception.Handler $ \se ->
    case Exception.fromException $ withoutCallStack se of
      Just e -> f e
      Nothing -> Exception.throwM se
  ]

-- | Throws an exception with a call stack. This wraps 'withCallStack' and
-- should be preferred over 'Exception.throwM' whenever possible.
throw
  :: (Stack.HasCallStack, Exception.Exception e, Exception.MonadThrow m)
  => e
  -> m a
throw = Exception.throwM . withCallStack . Exception.toException

-- | Adds a call stack if there isn't one already. Whatever calls this function
-- should probably have a 'Stack.HasCallStack' constraint. Instead of calling
-- this function directly, consider calling 'throw' instead.
withCallStack
  :: Stack.HasCallStack => Exception.SomeException -> Exception.SomeException
withCallStack x = case Exception.fromException x of
  Just (WithCallStack _ (Exception.SomeException _)) -> x
  Nothing -> Exception.toException WithCallStack
    { callStack = Stack.popCallStack Stack.callStack
    , value = x
    }

-- | Removes any call stacks. Instead of calling this function directly,
-- consider using 'catch' instead.
withoutCallStack :: Exception.SomeException -> Exception.SomeException
withoutCallStack e1 = case Exception.fromException e1 of
  Just (WithCallStack _ e2) -> withoutCallStack e2
  Nothing -> e1

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.WithCallStack" $ do

  Hspec.describe "catch" $ do

    Hspec.it "catches an exception without a call stack" $ do
      catch
        (Exception.throwM TestException.TestException)
        (`Hspec.shouldBe` TestException.TestException)

    Hspec.it "catches an exception with a call stack" $ do
      catch
        (throw TestException.TestException)
        (`Hspec.shouldBe` TestException.TestException)

  Hspec.describe "throw" $ do

    Hspec.it "adds a call stack" $ do
      throw TestException.TestException
        `Hspec.shouldThrow` (== Just TestException.TestException)
        . Exception.fromException
        . value

  Hspec.describe "withCallStack" $ do

    Hspec.it "adds a call stack" $ do
      let
        x :: Maybe TestException.TestException
        x =
          Monad.join
            . fmap (Exception.fromException . value)
            . Exception.fromException
            . withCallStack
            $ Exception.toException TestException.TestException
      x `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "does not add two call stacks" $ do
      let
        x :: Maybe TestException.TestException
        x =
          Monad.join
            . fmap (Exception.fromException . value)
            . Exception.fromException
            . withCallStack
            . withCallStack
            $ Exception.toException TestException.TestException
      x `Hspec.shouldSatisfy` Maybe.isJust

  -- Testing this is tough because it uses @SomeException@, which doesn't have
  -- an @Eq@ instance. Fortunately this behavior is tested by the @catch@
  -- tests.
  Hspec.describe "withoutCallStack" $ pure ()
