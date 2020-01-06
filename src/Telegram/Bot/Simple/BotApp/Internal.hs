{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Bot.Simple.BotApp.Internal where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad (forM_, void)
import Control.Monad.Error.Class
import Control.Monad.Trans (liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import ForkForever
import Servant.Client (ClientEnv, ClientM, runClientM)
import qualified System.Cron as Cron
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.Eff
import Text.Show.Pretty (ppShow)
import Time

-- | A bot application.
data BotApp model action
  = BotApp
      { -- | Initial bot state.
        botInitialModel :: model,
        -- | How to convert incoming 'Telegram.Update's into @action@s.
        -- See "Telegram.Bot.Simple.UpdateParser" for some helpers.
        botAction :: Telegram.Update -> model -> Maybe action,
        -- | How to handle @action@s.
        botHandler :: action -> model -> Eff action model,
        -- | Background bot jobs.
        botJobs :: [BotJob model action],
        botErrorHandlers :: [Handler BotM action]
      }

-- | A background bot job.
data BotJob model action
  = BotJob
      { -- | Cron schedule for the job.
        botJobSchedule :: Text,
        -- | Job function.
        botJobTask :: model -> Eff action model
      }

-- | An environment actual bot runs in.
data BotEnv model action
  = BotEnv
      { -- | A transactional variable with bot's current state.
        botModelVar :: TVar model,
        -- | A queue of @action@s to process (with associated 'Telegram.Update's).
        botActionsQueue :: TQueue (Maybe Telegram.Update, action),
        -- | HTTP client environment (where and how exactly to make requests to Telegram Bot API).
        -- This includes 'Telegram.Token'.
        botClientEnv :: ClientEnv,
        -- | Information about the bot in the form of 'Telegram.User'.
        botUser :: Telegram.User
      }

instance Functor (BotJob model) where
  fmap f BotJob {..} = BotJob {botJobTask = first f . botJobTask, ..}

-- | Run bot job task once.
runJobTask :: BotEnv model action -> (model -> Eff action model) -> IO ()
runJobTask botEnv@BotEnv {..} task = do
  effects <- liftIO $ atomically $ do
    model <- readTVar botModelVar
    case runEff (task model) of
      (newModel, effects) -> do
        writeTVar botModelVar newModel
        return effects
  res <-
    flip runClientM botClientEnv $
      mapM_ ((>>= liftIO . issueAction botEnv Nothing) . runBotM (BotContext botUser Nothing)) effects
  case res of
    Left err -> print $ "Job error: " <> ppShow err
    Right _ -> return ()

-- | Schedule a cron-like bot job.
scheduleBotJob :: BotEnv model action -> BotJob model action -> IO [ThreadId]
scheduleBotJob botEnv BotJob {..} =
  Cron.execSchedule $
    Cron.addJob (runJobTask botEnv botJobTask) botJobSchedule

-- | Schedule all bot jobs.
scheduleBotJobs :: BotEnv model action -> [BotJob model action] -> IO [ThreadId]
scheduleBotJobs botEnv jobs =
  concat
    <$> traverse (scheduleBotJob botEnv) jobs

-- | Construct a default @'BotEnv' model action@ for a bot.
defaultBotEnv :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
defaultBotEnv BotApp {..} env =
  BotEnv
    <$> newTVarIO botInitialModel
    <*> newTQueueIO
    <*> pure env
    <*> (either (error . show) Telegram.responseResult <$> runClientM Telegram.getMe env)

-- | Issue a new action for the bot to process.
issueAction :: BotEnv model action -> Maybe Telegram.Update -> action -> IO ()
issueAction BotEnv {..} update action =
  atomically $
    writeTQueue botActionsQueue (update, action)

-- | Process one action.
processAction ::
  BotApp model action ->
  BotEnv model action ->
  Maybe Telegram.Update ->
  action ->
  ClientM ()
processAction BotApp {..} botEnv@BotEnv {..} update action = do
  effects <- liftIO $ atomically $ do
    model <- readTVar botModelVar
    case runEff (botHandler action model) of
      (newModel, effects) -> do
        writeTVar botModelVar newModel
        return effects
  mapM_ issueActionIfPossible =<< mapM runBot effects
  where
    issueActionIfPossible (Just act) = liftIO $ issueAction botEnv update act
    issueActionIfPossible Nothing = return ()
    botCtx = BotContext botUser update
    runBot act =
      runBotM botCtx $
        fmap Just act
          `catches` (fmap Just <$> botErrorHandlers)
          `catchError` throw
          `catchAny` ( \err -> do
                         liftIO (print $ "Action error: " <> ppShow err)
                         return Nothing
                     )

-- | A job to wait for the next action and process it.
processActionJob :: BotApp model action -> BotEnv model action -> ClientM ()
processActionJob botApp botEnv@BotEnv {..} = do
  (update, action) <- liftIO . atomically $ readTQueue botActionsQueue
  processAction botApp botEnv update action

-- | Process incoming actions indefinitely.
processActionsIndefinitely ::
  BotApp model action -> BotEnv model action -> IO ThreadId
processActionsIndefinitely botApp botEnv =
  forkForever
    $ void
    $ runClientM (processActionJob botApp botEnv) (botClientEnv botEnv)

-- | Start 'Telegram.Update' polling for a bot.
startBotPolling ::
  forall (unit :: Rat) model action.
  (KnownDivRat unit Microsecond) =>
  Time unit ->
  BotApp model action ->
  BotEnv model action ->
  ClientM ()
startBotPolling period BotApp {..} botEnv@BotEnv {..} =
  startPolling period handleUpdate `catchAny` (\(e :: SomeException) -> liftIO (print $ "GetUpdates failed: " <> ppShow e))
  where
    handleUpdate update = void . liftIO . forkIO $ do
      maction <- botAction update <$> readTVarIO botModelVar
      forM_ maction (issueAction botEnv (Just update))

-- | Start 'Telegram.Update' polling with a given update handler.
startPolling ::
  forall (unit :: Rat).
  (KnownDivRat unit Microsecond) =>
  Time unit ->
  (Telegram.Update -> ClientM ()) ->
  ClientM ()
startPolling period handleUpdate = go Nothing
  where
    go lastUpdateId = do
      let inc (Telegram.UpdateId n) = Telegram.UpdateId (n + 1)
          offset = fmap inc lastUpdateId
      res <- Telegram.getUpdates (Telegram.GetUpdatesRequest offset Nothing Nothing Nothing)
      nextUpdateId <- do
        let updates = Telegram.responseResult res
            updateIds = map Telegram.updateUpdateId updates
            maxUpdateId = maximum (Nothing : map Just updateIds)
        mapM_ handleUpdate updates
        pure maxUpdateId
      liftIO $ threadDelay period
      go nextUpdateId
