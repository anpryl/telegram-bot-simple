module ServantClient where

import Control.Exception.Safe
import Servant.Client

runClientWithException :: ClientM a -> ClientEnv -> IO a
runClientWithException act env =
  either throwM pure =<< runClientM act env
