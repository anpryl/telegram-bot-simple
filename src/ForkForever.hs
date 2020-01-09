module ForkForever where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Monad
import Data.Function (fix)

forkForeverWithName :: String -> IO a -> IO ThreadId
forkForeverWithName name act = forkIO $ fix $ \next -> do
  withAsync act (waitCatch >=> either printMsgOnException (const $ pure ()))
  next
  where
    one x = [x]
    printMsgOnException =
      print . unwords . (["Thread", name, "died:"] <>) . one . show

forkForeverWithName_ :: String -> IO a -> IO ()
forkForeverWithName_ name = void . forkForeverWithName_ name

forkForever :: IO a -> IO ThreadId
forkForever = forkForeverWithName mempty

forkForever_ :: IO a -> IO ()
forkForever_ = void . forkForever
