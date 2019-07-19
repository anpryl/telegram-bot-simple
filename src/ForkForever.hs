module ForkForever where

import           Control.Concurrent     (ThreadId, forkIO)
import           Control.Exception.Safe (catchAny)
import           Control.Monad          (void)
import           Data.Function          (fix)

forkForever_ :: IO a -> IO ()
forkForever_ act = forkForever act >> pure ()

forkForever :: IO a -> IO ThreadId
forkForever act = forkIO $ fix $ \next -> do
    void act `catchAny` (print . ("Thread died: " <>) . show)
    next
