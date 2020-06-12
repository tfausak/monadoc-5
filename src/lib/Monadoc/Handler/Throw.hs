module Monadoc.Handler.Throw
  ( handle
  )
where

import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.WithCallStack as WithCallStack

handle :: App.App request result
handle = WithCallStack.throw $ userError "oh no"
