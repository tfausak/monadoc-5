module Monadoc.Utility.Console where

import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Data.Time as Time
import qualified Monadoc.Utility.Time as Time
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

-- | Puts a timestamp in front of the given message and logs it to STDOUT.
-- Ensures that only one thread outputs at a time, so output won't be garbled
-- when running on multiple threads.
info :: IO.MonadIO m => String -> m ()
info = logOn IO.stdout

-- | Just like 'info' but on STDERR instead of STDOUT.
warn :: IO.MonadIO m => String -> m ()
warn = logOn IO.stderr

logOn :: IO.MonadIO m => IO.Handle -> String -> m ()
logOn handle message = do
  now <- IO.liftIO Time.getCurrentTime
  IO.liftIO
    . Exception.bracket
        (Stm.atomically $ Stm.takeTMVar logVar)
        (Stm.atomically . Stm.putTMVar logVar)
    $ \() -> IO.liftIO . IO.hPutStrLn handle $ unwords
        [Time.format "%Y-%m-%dT%H:%M:%S%3QZ" now, message]

logVar :: Stm.TMVar ()
logVar = Unsafe.unsafePerformIO $ Stm.newTMVarIO ()
{-# NOINLINE logVar #-}
