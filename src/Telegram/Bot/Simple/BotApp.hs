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
startBotAsync bot env = do
  botEnv <- startBotEnv bot env
  forkForever_ $ runClientM (startBotPolling bot botEnv) env
  return (issueAction botEnv Nothing)

-- | Like 'startBotAsync', but ignores result.
startBotAsync_ :: BotApp model action -> ClientEnv -> IO ()
startBotAsync_ bot env = void (startBotAsync bot env)

-- | Start bot with update polling in the main thread.
startBot :: BotApp model action -> ClientEnv -> IO (Either ServantError ())
startBot bot env = do
  botEnv <- startBotEnv bot env
  runClientM (startBotPolling bot botEnv) env `onException` (putStrLn "HI")

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

startBotEnv :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
startBotEnv bot env = do
  botEnv <- defaultBotEnv bot env
  _jobThreadIds <- scheduleBotJobs botEnv (botJobs bot)
  _actionsThreadId <- processActionsIndefinitely bot botEnv
  return botEnv
