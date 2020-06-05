module Monadoc.Type.WithCallStack
  ( WithCallStack(..)
  , catch
  , throw
  , withCallStack
  )
where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Stack as Stack

-- | Some value with a 'Stack.CallStack' attached. Typically this is used with
-- 'Exception.SomeException' to attach call stacks to exceptions.
data WithCallStack a = WithCallStack
  { callStack :: Stack.CallStack
  , value :: a
  } deriving Show

instance Eq a => Eq (WithCallStack a) where
  x == y =
    Stack.getCallStack (callStack x) == Stack.getCallStack (callStack y) &&
    value x == value y

instance Exception.Exception e => Exception.Exception (WithCallStack e) where
  displayException x =
    Exception.displayException (value x)
    <> "\n"
    <> Stack.prettyCallStack (callStack x)

-- | Catches an exception, removing call stacks as necessary. This wraps
-- 'value' to make it easy to catch an exception without a call stack even if
-- it was thrown with one. You should prefer this over 'Exception.catch' when
-- possible.
catch :: (Exception.MonadCatch m, Exception.Exception e) => m a -> (e -> m a) -> m a
catch x f = Exception.catches x
  [ Exception.Handler f
  , Exception.Handler $ \ e ->
    case Exception.fromException $ value e of
      Just y -> f y
      _ -> Exception.throwM e
  ]

-- | Throws an exception with a call stack. This wraps 'withCallStack' and
-- should be preferred over 'Exception.throwM' whenever possible.
throw :: (Stack.HasCallStack, Exception.Exception e, Exception.MonadThrow m) => e -> m a
throw = Exception.throwM . withCallStack . Exception.toException

-- | Adds a call stack if there isn't one already. Whatever calls this function
-- should probably have a 'Stack.HasCallStack' constraint. Instead of calling
-- this function directly, consider calling 'throw' instead.
withCallStack :: Stack.HasCallStack => Exception.SomeException -> Exception.SomeException
withCallStack x = case Exception.fromException x of
  Just (WithCallStack _ (Exception.SomeException _)) -> x
  Nothing -> Exception.toException WithCallStack { callStack = Stack.callStack, value = x }
