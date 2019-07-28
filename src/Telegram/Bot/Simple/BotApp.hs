module Telegram.Bot.Simple.BotApp (
  BotApp(..),
  BotJob(..),

  startBot,
  startBot_,

  startBotAsync,
  startBotAsync_,

  getEnvToken,
) where

import           Control.Monad                       (void)
import           Control.Concurrent                  (ThreadId, killThread)
import           Control.Exception.Safe
import           Data.String                         (fromString)
import           ForkForever
import           Servant.Client
import           System.Environment                  (getEnv)

import qualified Telegram.Bot.API                    as Telegram
import           Telegram.Bot.Simple.BotApp.Internal

-- | Start bot with asynchronous polling.
-- The result is a function that allows you to send actions
-- directly to the bot.
startBotAsync :: BotApp model action -> ClientEnv -> IO (action -> IO ())
startBotAsync bot env = withBotEnv bot env $ \botEnv -> do
  forkForever_ $ runClientM (startBotPolling bot botEnv) env
  return (issueAction botEnv Nothing)

-- | Like 'startBotAsync', but ignores result.
startBotAsync_ :: BotApp model action -> ClientEnv -> IO ()
startBotAsync_ bot env = void (startBotAsync bot env)

-- | Start bot with update polling in the main thread.
startBot :: BotApp model action -> ClientEnv -> IO (Either ServantError ())
startBot bot env = withBotEnv bot env $ \botEnv -> do
  runClientM (startBotPolling bot botEnv) env

-- | Like 'startBot', but ignores result.
startBot_ :: BotApp model action -> ClientEnv -> IO ()
startBot_ bot = void . startBot bot

-- | Get a 'Telegram.Token' from environment variable.
--
-- Common use:
--
-- @
-- 'getEnvToken' "TELEGRAM_BOT_TOKEN"
-- @
getEnvToken :: String -> IO Telegram.Token
getEnvToken varName = fromString <$> getEnv varName

withBotEnv :: BotApp model action -> ClientEnv -> (BotEnv model action -> IO a) -> IO a
withBotEnv bot env act = do
  (threadIDs, botEnv) <- startBotEnv bot env
  act botEnv `onException` traverse killThread threadIDs

startBotEnv :: BotApp model action -> ClientEnv -> IO ([ThreadId], BotEnv model action)
startBotEnv bot env = do
  botEnv <- defaultBotEnv bot env
  jobThreadIds <- scheduleBotJobs botEnv (botJobs bot)
  actionsThreadId <- processActionsIndefinitely bot botEnv
  return (jobThreadIds <> [actionsThreadId], botEnv)
