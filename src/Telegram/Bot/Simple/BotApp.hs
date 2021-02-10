{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Telegram.Bot.Simple.BotApp (
    BotApp (..),
    BotJob (..),
    startBot,
    startBot_,
    startBotAsync,
    startBotAsync_,
    getEnvToken,
    defaultPeriod,
) where

import Control.Concurrent (ThreadId, killThread)
import Control.Exception.Safe
import Control.Monad (void)
import Data.String (fromString)
import ForkForever
import Servant.Client
import ServantClient
import System.Environment (getEnv)
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.BotApp.Internal
import Time

defaultPeriod :: Time Second
defaultPeriod = Time @Second 10

{- | Start bot with asynchronous polling.
 The result is a function that allows you to send actions
 directly to the bot.
-}
startBotAsync ::
    forall (unit :: Rat) model action.
    (KnownDivRat unit Microsecond) =>
    Time unit ->
    BotApp model action ->
    ClientEnv ->
    IO (action -> IO ())
startBotAsync period bot env = withBotEnv bot env $ \botEnv -> do
    forkForeverWithName_ forkName (runClient botEnv) forkErrorHandler
    return (issueAction botEnv Nothing)
  where
    forkName = "startBotAsync"
    runClient botEnv = runClientWithException (startBotPolling period bot botEnv) env
    forkErrorHandler = botForkErrorHandler bot

-- | Like 'startBotAsync', but ignores result.
startBotAsync_ ::
    forall (unit :: Rat) model action.
    (KnownDivRat unit Microsecond) =>
    Time unit ->
    BotApp model action ->
    ClientEnv ->
    IO ()
startBotAsync_ period bot env = void (startBotAsync period bot env)

-- | Start bot with update polling in the main thread.
startBot ::
    forall (unit :: Rat) model action.
    (KnownDivRat unit Microsecond) =>
    Time unit ->
    BotApp model action ->
    ClientEnv ->
    IO (Either ClientError ())
startBot period bot env = withBotEnv bot env $ \botEnv ->
    runClientM (startBotPolling period bot botEnv) env

-- | Like 'startBot', but ignores result.
startBot_ ::
    forall (unit :: Rat) model action.
    (KnownDivRat unit Microsecond) =>
    Time unit ->
    BotApp model action ->
    ClientEnv ->
    IO ()
startBot_ period bot = void . startBot period bot

{- | Get a 'Telegram.Token' from environment variable.

 Common use:

 @
 'getEnvToken' "TELEGRAM_BOT_TOKEN"
 @
-}
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
