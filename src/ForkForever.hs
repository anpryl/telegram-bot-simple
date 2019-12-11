module ForkForever where

import           Control.Concurrent       (ThreadId)
import           Control.Concurrent.Async (waitCatch, withAsync)
import           Data.Function            (fix)

forkForever_ :: IO a -> IO ()
forkForever_ act = forkForever act >> pure ()

forkForever :: IO a -> IO ThreadId
forkForever act = fix $ \next -> do
  withAsync act $ \a -> waitCatch a >>= either (print . ("Thread died: " <>) . show) (const $ pure ())
  next
