module Monadoc.Handler.Throw where

import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack

import Prelude ()
-- import Monadoc.Prelude

handle :: App.App request result
handle = WithCallStack.throw TestException.TestException
