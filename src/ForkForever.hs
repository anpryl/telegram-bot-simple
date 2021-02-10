module ForkForever where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Exception.Safe
import Control.Monad
import Data.Function (fix)

type ForkName = String

type ForkExceptionHandler = Maybe (ForkName -> SomeException -> IO ())

forkForeverWithName :: ForkName -> IO a -> ForkExceptionHandler -> IO ThreadId
forkForeverWithName name act mhandler = forkIO $
    fix $ \next -> do
        withAsync act (waitCatch >=> either handleExceptions (const $ pure ()))
        next
  where
    handleExceptions e = do
        printMsgOnException e
        case mhandler of
            Nothing -> pure ()
            Just handler -> handler name e `catchAsync` printMsgOnException
    one x = [x]
    printMsgOnException =
        print . unwords . (["Thread", name, "died:"] <>) . one . show

forkForeverWithName_ :: ForkName -> IO a -> ForkExceptionHandler -> IO ()
forkForeverWithName_ name act = void . forkForeverWithName name act

forkForever :: IO a -> ForkExceptionHandler -> IO ThreadId
forkForever = forkForeverWithName mempty

forkForever_ :: IO a -> ForkExceptionHandler -> IO ()
forkForever_ act = void . forkForever act
