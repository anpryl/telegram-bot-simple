module Telegram.Bot.Simple.ServantClient
  ( runClientWithException
  ) where

import Control.Exception.Safe (throwIO)
import Servant.Client (ClientEnv, ClientM, runClientM)

-- | Run a 'ClientM' action and throw on failure instead of returning 'Either'.
--
-- This replaces the common pattern of:
--
-- @
-- res <- runClientM action env
-- case res of
--   Left err -> print err
--   Right _  -> return ()
-- @
--
-- Exceptions are handled by the caller — typically an immortal worker
-- that logs the error and restarts, or by 'botErrorHandlers'.
runClientWithException :: ClientM a -> ClientEnv -> IO a
runClientWithException act env = do
  res <- runClientM act env
  case res of
    Left err -> throwIO err
    Right v  -> pure v
