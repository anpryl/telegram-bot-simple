{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Telegram.Bot.Simple.BotApp.Internal where

import           Control.Concurrent          (ThreadId, forkIO)
import           Control.Concurrent.STM
import           Control.Exception.Safe      (Handler, catches, throw)
import           Control.Monad               (forM_, void, (<=<))
import           Control.Monad.Error.Class   (catchError)
import           Control.Monad.Logger        (MonadLogger)
import           Control.Monad.Trans         (liftIO)
import           Data.Aeson.Types            (parseEither, parseJSON)
import           Data.Bifunctor              (first)
import           Data.Either                 (partitionEithers)
import           Data.Text                   (Text)
import           Servant.Client              (ClientEnv, ClientM, runClientM)
import qualified System.Cron                 as Cron
import           Text.Show.Pretty            (ppShow)
import           Time                        (KnownDivRat, Microsecond, Rat, Time,
                                              threadDelay)
import           UnliftIO                    (MonadUnliftIO)

import qualified Control.Immortal.Worker     as I
import qualified Telegram.Bot.API            as Telegram
import           Telegram.Bot.Simple.Eff
import           Telegram.Bot.Simple.ServantClient (runClientWithException)

-- | A bot application.
data BotApp model action = BotApp
  { botInitialModel  :: model
    -- ^ Initial bot state.
  , botAction        :: Telegram.Update -> model -> Maybe action
    -- ^ How to convert incoming 'Telegram.Update's into @action@s.
    -- See "Telegram.Bot.Simple.UpdateParser" for some helpers.
  , botHandler       :: action -> model -> Eff action model
    -- ^ How to handle @action@s.
  , botJobs          :: [BotJob model action]
    -- ^ Background bot jobs.
  , botErrorHandlers :: [Handler BotM action]
    -- ^ Exception handlers for bot action processing.
    -- When an action handler throws, these handlers can recover
    -- by returning a new action to process, or re-throw.
    -- Default: @[]@ (no custom error handling).
  }

-- | A background bot job.
data BotJob model action = BotJob
  { botJobSchedule :: Text
    -- ^ Cron schedule for the job.
  , botJobTask     :: model -> Eff action model
    -- ^ Job function.
  }

-- | An environment actual bot runs in.
data BotEnv model action = BotEnv
  { botModelVar     :: TVar model
    -- ^ A transactional variable with bot's current state.
  , botActionsQueue :: TQueue (Maybe Telegram.Update, action)
    -- ^ A queue of @action@s to process (with associated 'Telegram.Update's).
  , botClientEnv    :: ClientEnv
    -- ^ HTTP client environment (where and how exactly to make requests to Telegram Bot API).
    -- This includes 'Telegram.Token'.
  , botUser         :: Telegram.User
    -- ^ Information about the bot in the form of 'Telegram.User'.
  }

instance Functor (BotJob model) where
  fmap f BotJob{..} = BotJob{ botJobTask = first f . botJobTask, .. }

-- | Run bot job task once.
runJobTask :: BotEnv model action -> (model -> Eff action model) -> IO ()
runJobTask botEnv@BotEnv{..} task = do
  effects <- liftIO $ atomically $ do
    model <- readTVar botModelVar
    case runEff (task model) of
      (newModel, effects) -> do
        writeTVar botModelVar newModel
        return effects
  res <- flip runClientM botClientEnv $
    mapM_ ((liftIO . issueAction botEnv Nothing) <=< runBotM (BotContext botUser Nothing)) effects
  case res of
    Left err -> putStrLn $ "Job error: " <> ppShow err
    Right _  -> return ()

-- | Schedule a cron-like bot job.
scheduleBotJob :: BotEnv model action -> BotJob model action -> IO [ThreadId]
scheduleBotJob botEnv BotJob{..} = Cron.execSchedule $ do
  Cron.addJob (runJobTask botEnv botJobTask) botJobSchedule

-- | Schedule all bot jobs.
scheduleBotJobs :: BotEnv model action -> [BotJob model action] -> IO [ThreadId]
scheduleBotJobs botEnv jobs = concat
  <$> traverse (scheduleBotJob botEnv) jobs

-- | Construct a default @'BotEnv' model action@ for a bot.
defaultBotEnv :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
defaultBotEnv BotApp{..} env = BotEnv
  <$> newTVarIO botInitialModel
  <*> newTQueueIO
  <*> pure env
  <*> (Telegram.responseResult <$> runClientWithException Telegram.getMe env)

-- | Issue a new action for the bot to process.
issueAction :: BotEnv model action -> Maybe Telegram.Update -> Maybe action -> IO ()
issueAction BotEnv{..} update (Just action) = atomically $
  writeTQueue botActionsQueue (update, action)
issueAction _ _ _ = pure ()

-- | Process one action.
--
-- If 'botErrorHandlers' are defined, exceptions thrown during action
-- processing are caught and routed through the handlers, which can
-- return a recovery action or re-throw.
processAction
  :: BotApp model action
  -> BotEnv model action
  -> Maybe Telegram.Update
  -> action
  -> ClientM ()
processAction BotApp{..} botEnv@BotEnv{..} update action = do
  effects <- liftIO $ atomically $ do
    model <- readTVar botModelVar
    case runEff (botHandler action model) of
      (newModel, effects) -> do
        writeTVar botModelVar newModel
        return effects
  mapM_ runBotAndIssueAction effects
  where
    botCtx = BotContext botUser update
    runBotAndIssueAction act =
      (liftIO . issueAction botEnv update) =<< runBotM botCtx
        (act `catchError` throw `catches` botErrorHandlers)

-- | A job to wait for the next action and process it.
processActionJob :: BotApp model action -> BotEnv model action -> ClientM ()
processActionJob botApp botEnv@BotEnv{..} = do
  (update, action) <- liftIO . atomically $ readTQueue botActionsQueue
  processAction botApp botEnv update action

-- | Process incoming actions indefinitely.
--
-- Uses an immortal worker thread that automatically restarts on failure,
-- instead of 'asyncLink' which propagates exceptions to the parent.
processActionsIndefinitely
  :: (MonadLogger m, MonadUnliftIO m)
  => BotApp model action -> BotEnv model action -> m I.Thread
processActionsIndefinitely botApp botEnv =
  I.worker "TelegramBotSimple.processActionsIndefinitely" $ const $ liftIO runClient
  where
    runClient = runClientWithException (processActionJob botApp botEnv) (botClientEnv botEnv)

-- | Start 'Telegram.Update' polling for a bot.
startBotPolling
  :: forall (unit :: Rat) model action.
     KnownDivRat unit Microsecond
  => Time unit
  -> BotApp model action
  -> BotEnv model action
  -> ClientM ()
startBotPolling period BotApp{..} botEnv@BotEnv{..} =
  startPolling period handleUpdate
  where
    handleUpdate update = liftIO . void . forkIO $ do
      maction <- botAction update <$> readTVarIO botModelVar
      forM_ maction (\act -> issueAction botEnv (Just update) (Just act))

-- | Start 'Telegram.Update' polling with a given update handler.
startPolling
  :: forall (unit :: Rat).
     KnownDivRat unit Microsecond
  => Time unit
  -> (Telegram.Update -> ClientM ())
  -> ClientM ()
startPolling period handleUpdate = go Nothing
  where
    go lastUpdateId = do
      let inc (Telegram.UpdateId n) = Telegram.UpdateId (n + 1)
          offset = fmap inc lastUpdateId
      res <-
        (Right <$> Telegram.getUpdatesAsValue
          (Telegram.GetUpdatesRequest offset Nothing (Just 25) Nothing))
        `catchError` (pure . Left)

      nextUpdateId <- case res of
        Left servantErr -> do
          liftIO (putStrLn $ "Polling error: " <> ppShow servantErr)
          pure lastUpdateId
        Right result -> do
          let updateValues = Telegram.responseResult result
              (errors, updates) = parseUpdates updateValues
              updateIds = map Telegram.updateUpdateId updates
              maxUpdateId = maximum (Nothing : map Just updateIds)
          mapM_ reportParseError errors
          mapM_ handleUpdate updates
          pure maxUpdateId
      liftIO $ threadDelay period
      go nextUpdateId

    parseUpdates updates =
      partitionEithers (map (parseEither parseJSON) updates)

    reportParseError err =
      liftIO $ putStrLn $
        "Failed to parse an update! Please, make sure you have the latest version of `telegram-bot-api`\
        \ library and consider opening an issue if so. Error message: " <> err
