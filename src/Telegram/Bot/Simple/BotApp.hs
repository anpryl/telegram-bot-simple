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

import Control.Immortal.Worker as I
import Control.Monad (void)
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Data.String (fromString)
import Servant.Client
import ServantClient
import System.Environment (getEnv)
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.BotApp.Internal
import Time
import UnliftIO

defaultPeriod :: Time Second
defaultPeriod = Time @Second 10

{- | Start bot with asynchronous polling.
 The result is a function that allows you to send actions
 directly to the bot.
-}
startBotAsync ::
    forall (unit :: Rat) model action m.
    (KnownDivRat unit Microsecond) =>
    MonadLogger m =>
    MonadUnliftIO m =>
    Time unit ->
    BotApp model action ->
    ClientEnv ->
    m (action -> IO ())
startBotAsync period bot env = withBotEnv bot env $ \botEnv -> do
    _ <- I.worker "TelegramBotSimple.startBotAsync" $ const $ liftIO $ runClient botEnv
    return (issueAction botEnv Nothing)
  where
    runClient botEnv = runClientWithException (startBotPolling period bot botEnv) env

-- | Like 'startBotAsync', but ignores result.
startBotAsync_ ::
    forall (unit :: Rat) model action m.
    (KnownDivRat unit Microsecond) =>
    MonadLogger m =>
    MonadUnliftIO m =>
    Time unit ->
    BotApp model action ->
    ClientEnv ->
    m ()
startBotAsync_ period bot env = void (startBotAsync period bot env)

-- | Start bot with update polling in the main thread.
startBot ::
    forall (unit :: Rat) model action.
    (KnownDivRat unit Microsecond) =>
    Time unit ->
    BotApp model action ->
    ClientEnv ->
    LoggingT IO (Either ClientError ())
startBot period bot env = withBotEnv bot env $ \botEnv ->
    lift $ runClientM (startBotPolling period bot botEnv) env

-- | Like 'startBot', but ignores result.
startBot_ ::
    forall (unit :: Rat) model action.
    (KnownDivRat unit Microsecond) =>
    Time unit ->
    BotApp model action ->
    ClientEnv ->
    LoggingT IO ()
startBot_ period bot = void . startBot period bot

{- | Get a 'Telegram.Token' from environment variable.

 Common use:

 @
 'getEnvToken' "TELEGRAM_BOT_TOKEN"
 @
-}
getEnvToken :: String -> IO Telegram.Token
getEnvToken varName = fromString <$> getEnv varName

withBotEnv ::
    MonadLogger m =>
    MonadUnliftIO m =>
    BotApp model action ->
    ClientEnv ->
    (BotEnv model action -> m a) ->
    m a
withBotEnv bot env act = do
    botEnv <- startBotEnv bot env
    act botEnv

startBotEnv ::
    MonadLogger m =>
    MonadUnliftIO m =>
    BotApp model action ->
    ClientEnv ->
    m (BotEnv model action)
startBotEnv bot env = do
    botEnv <- liftIO $ defaultBotEnv bot env
    _ <- liftIO $ scheduleBotJobs botEnv (botJobs bot)
    _ <- processActionsIndefinitely bot botEnv
    return botEnv
