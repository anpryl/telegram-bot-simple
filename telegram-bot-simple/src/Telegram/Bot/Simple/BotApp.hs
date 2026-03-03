{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications   #-}
module Telegram.Bot.Simple.BotApp (
  BotApp(..),
  BotJob(..),
  WebhookConfig(..),

  defaultPeriod,

  startBot,
  startBot_,

  startBotAsync,
  startBotAsync_,

  startBotWebhook,
  startBotWebhook_,

  getEnvToken,
) where

import           Control.Exception                   (finally)
import           Control.Monad                       (void)
import           Control.Monad.Logger                (MonadLogger, runNoLoggingT)
import           Data.Either                         (isLeft)
import           Data.String                         (fromString)
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Servant.Client
import           System.Environment                  (getEnv)
import           Time                                (KnownDivRat, Microsecond,
                                                      Rat, Second, Time (..))
import           UnliftIO                            (MonadUnliftIO, liftIO)

import qualified Control.Immortal.Worker             as I
import qualified Telegram.Bot.API                    as Telegram
import           Telegram.Bot.API.Webhook            (SetWebhookRequest,
                                                      deleteWebhook,
                                                      setUpWebhook)
import           Telegram.Bot.Simple.BotApp.Internal
import           Telegram.Bot.Simple.ServantClient    (runClientWithException)
import           Telegram.Bot.Simple.Webhook         (webhookApp)

-- | Default polling period (10 seconds).
defaultPeriod :: Time Second
defaultPeriod = Time @Second 10

-- | Start bot with asynchronous polling.
-- The result is a function that allows you to send actions
-- directly to the bot.
--
-- Uses immortal worker threads that automatically restart on failure.
startBotAsync
  :: forall (unit :: Rat) model action m.
     ( KnownDivRat unit Microsecond
     , MonadLogger m
     , MonadUnliftIO m
     )
  => Time unit
  -> BotApp model action
  -> ClientEnv
  -> m (action -> m ())
startBotAsync period bot env = withBotEnv bot env $ \botEnv -> do
  _ <- I.worker "TelegramBotSimple.startBotAsync" $ const $
    liftIO $ runClientWithException (startBotPolling period bot botEnv) env
  return (liftIO . issueAction botEnv Nothing . Just)

-- | Like 'startBotAsync', but ignores result.
startBotAsync_
  :: forall (unit :: Rat) model action m.
     ( KnownDivRat unit Microsecond
     , MonadLogger m
     , MonadUnliftIO m
     )
  => Time unit
  -> BotApp model action
  -> ClientEnv
  -> m ()
startBotAsync_ period bot env = void (startBotAsync period bot env)

-- | Start bot with update polling in the main thread.
startBot
  :: forall (unit :: Rat) model action m.
     ( KnownDivRat unit Microsecond
     , MonadLogger m
     , MonadUnliftIO m
     )
  => Time unit
  -> BotApp model action
  -> ClientEnv
  -> m (Either ClientError ())
startBot period bot env = withBotEnv bot env $ \botEnv ->
  liftIO $ runClientM (startBotPolling period bot botEnv) env

-- | Like 'startBot', but ignores result.
startBot_
  :: forall (unit :: Rat) model action m.
     ( KnownDivRat unit Microsecond
     , MonadLogger m
     , MonadUnliftIO m
     )
  => Time unit
  -> BotApp model action
  -> ClientEnv
  -> m ()
startBot_ period bot = void . startBot period bot

data WebhookConfig = WebhookConfig
  { webhookConfigTlsSettings       :: TLSSettings
  , webhookConfigTlsWarpSettings   :: Settings
  , webhookConfigSetWebhookRequest :: SetWebhookRequest
  }

-- | Start bot with webhook on update in the main thread.
-- Port must be one of 443, 80, 88, 8443.
-- certPath must be provided if using self signed certificate.
--
-- Note: webhook mode uses IO directly and does not use immortal workers,
-- since warp manages its own thread lifecycle.
startBotWebhook :: BotApp model action -> WebhookConfig -> ClientEnv -> IO (Either ClientError ())
startBotWebhook bot (WebhookConfig{..}) env = do
  botEnv <- startBotEnvIO bot env
  res <- setUpWebhook webhookConfigSetWebhookRequest env
  if isLeft res
    then return res
    else Right <$> runTLS webhookConfigTlsSettings webhookConfigTlsWarpSettings (webhookApp bot botEnv)
  `finally`
    deleteWebhook env

-- | Like 'startBotWebhook', but ignores result.
startBotWebhook_ :: BotApp model action -> WebhookConfig -> ClientEnv -> IO ()
startBotWebhook_ bot webhookConfig = void . startBotWebhook bot webhookConfig

-- | Get a 'Telegram.Token' from environment variable.
--
-- Common use:
--
-- @
-- 'getEnvToken' "TELEGRAM_BOT_TOKEN"
-- @
getEnvToken :: String -> IO Telegram.Token
getEnvToken varName = fromString <$> getEnv varName

-- ** Internal helpers

withBotEnv
  :: (MonadLogger m, MonadUnliftIO m)
  => BotApp model action
  -> ClientEnv
  -> (BotEnv model action -> m a)
  -> m a
withBotEnv bot env act = do
  botEnv <- startBotEnv bot env
  act botEnv

startBotEnv
  :: (MonadLogger m, MonadUnliftIO m)
  => BotApp model action
  -> ClientEnv
  -> m (BotEnv model action)
startBotEnv bot env = do
  botEnv <- liftIO $ defaultBotEnv bot env
  _ <- liftIO $ scheduleBotJobs botEnv (botJobs bot)
  _ <- processActionsIndefinitely bot botEnv
  return botEnv

-- | Like 'startBotEnv' but in IO, for webhook mode.
-- Uses 'NoLoggingT' to satisfy 'MonadLogger' constraint.
startBotEnvIO :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
startBotEnvIO bot env = runNoLoggingT $ startBotEnv bot env
