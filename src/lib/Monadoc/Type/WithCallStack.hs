module Monadoc.Type.WithCallStack where

import qualified Control.Monad.Catch as Exception
import qualified Data.Function as Function
import qualified GHC.Stack as Stack

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
