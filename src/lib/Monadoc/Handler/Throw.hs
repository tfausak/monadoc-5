module Monadoc.Handler.Throw
  ( handle
  )
where

import qualified GHC.Stack as Stack
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack

handle :: Stack.HasCallStack => App.App request result
handle = WithCallStack.throw TestException.TestException
