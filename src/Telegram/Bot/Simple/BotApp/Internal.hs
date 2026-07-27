{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Telegram.Bot.Simple.BotApp.Internal where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception.Safe
-- Qualified as well as unqualified: 'UnliftIO' also exports 'catch' and
-- 'throw', so naming them here is ambiguous without a prefix.
import qualified Control.Exception.Safe as Safe
import Control.Immortal as I
import Control.Immortal.Worker as I
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Logger
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Servant.Client (ClientEnv, ClientM, runClientM)
import ServantClient
import qualified System.Cron as Cron
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.API.RetryAfter (retryAfterFromClientError)
import Telegram.Bot.Simple.Eff
import Text.Show.Pretty (ppShow)
import Time
import UnliftIO hiding (Handler, catchAny, catches, throwIO)

-- | A bot application.
data BotApp model action = BotApp
    { -- | Initial bot state.
      botInitialModel :: model
    , -- | How to convert incoming 'Telegram.Update's into @action@s.
      -- See "Telegram.Bot.Simple.UpdateParser" for some helpers.
      botAction :: Telegram.Update -> model -> Maybe action
    , -- | How to handle @action@s.
      botHandler :: action -> model -> Eff action model
    , -- | Background bot jobs.
      botJobs :: [BotJob model action]
    , -- | Handlers for exceptions
      botErrorHandlers :: [Handler BotM action]
    }

-- | A background bot job.
data BotJob model action = BotJob
    { -- | Cron schedule for the job.
      botJobSchedule :: Text
    , -- | Job function.
      botJobTask :: model -> Eff action model
    }

-- | An environment actual bot runs in.
data BotEnv model action = BotEnv
    { -- | A transactional variable with bot's current state.
      botModelVar :: TVar model
    , -- | A queue of @action@s to process (with associated 'Telegram.Update's).
      botActionsQueue :: TQueue (Maybe Telegram.Update, action)
    , -- | HTTP client environment (where and how exactly to make requests to Telegram Bot API).
      -- This includes 'Telegram.Token'.
      botClientEnv :: ClientEnv
    , -- | Information about the bot in the form of 'Telegram.User'.
      botUser :: Telegram.User
    }

instance Functor (BotJob model) where
    fmap f BotJob{..} = BotJob{botJobTask = first f . botJobTask, ..}

-- | Run bot job task once.
runJobTask :: BotEnv model action -> (model -> Eff action model) -> IO ()
runJobTask botEnv@BotEnv{..} task = do
    effects <- liftIO $
        atomically $ do
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
scheduleBotJob botEnv BotJob{..} =
    Cron.execSchedule $
        Cron.addJob (runJobTask botEnv botJobTask) botJobSchedule

-- | Schedule all bot jobs.
scheduleBotJobs :: BotEnv model action -> [BotJob model action] -> IO [ThreadId]
scheduleBotJobs botEnv jobs =
    concat
        <$> traverse (scheduleBotJob botEnv) jobs

-- | Construct a default @'BotEnv' model action@ for a bot.
defaultBotEnv :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
defaultBotEnv BotApp{..} env =
    BotEnv
        <$> newTVarIO botInitialModel
        <*> newTQueueIO
        <*> pure env
        <*> (Telegram.responseResult <$> runClientWithException Telegram.getMe env)

-- | Issue a new action for the bot to process.
issueAction :: BotEnv model action -> Maybe Telegram.Update -> action -> IO ()
issueAction BotEnv{..} update action =
    atomically $ writeTQueue botActionsQueue (update, action)

{- | How many times one outgoing Telegram call will wait out flood control
 before giving up on it.

 Bounded, unlike the poll loop, because this one is holding an action that has
 already been taken off the queue while everything behind it waits. Giving up
 costs one message; never giving up costs every message after it.
-}
maxFloodControlRetries :: Int
maxFloodControlRetries = 3

-- | Process one action.
processAction ::
    BotApp model action ->
    BotEnv model action ->
    (Text -> IO ()) ->
    Maybe Telegram.Update ->
    action ->
    ClientM ()
processAction BotApp{..} botEnv@BotEnv{..} logWarnIO update action = do
    effects <- liftIO $
        atomically $ do
            model <- readTVar botModelVar
            case runEff (botHandler action model) of
                (newModel, effects) -> do
                    writeTVar botModelVar newModel
                    return effects
    mapM_ (liftIO . issueAction botEnv update) =<< mapM (waitOutFloodControlThen . runBot) effects
  where
    botCtx = BotContext botUser update
    runBot act =
        runBotM botCtx $
            act `catchError` throw
                `catches` botErrorHandlers

    -- Retries ONE effect, which is the only granularity that is correct here.
    --
    -- Not the whole 'processAction': the model has already been updated by the
    -- 'atomically' block above, so re-running it would apply 'botHandler' to
    -- the new model a second time. Not the 'mapM' either: effect 3 failing
    -- would re-send effects 1 and 2. Only the individual call that was
    -- refused is safe to repeat.
    --
    -- Arrives as an exception rather than a 'MonadError' failure: 'runBot'
    -- above turns every 'ClientError' into one via @catchError throw@ before
    -- it ever reaches this point.
    waitOutFloodControlThen = go maxFloodControlRetries
      where
        go remaining send =
            send `Safe.catch` \err -> case retryAfterFromClientError err of
                Just retryAfter | remaining > 0 -> do
                    liftIO $ waitOutFloodControl logWarnIO "a bot reply" retryAfter
                    go (remaining - 1) send
                _ -> Safe.throw err

-- | A job to wait for the next action and process it.
processActionJob :: BotApp model action -> BotEnv model action -> (Text -> IO ()) -> ClientM ()
processActionJob botApp botEnv@BotEnv{..} logWarnIO = do
    (update, action) <- liftIO . atomically $ readTQueue botActionsQueue
    processAction botApp botEnv logWarnIO update action

{- | Process incoming actions indefinitely.

 Note that the read above commits before the send below is attempted, so an
 action that escapes 'processAction' is gone: the thread dies holding it and
 the restart begins from an empty hand. That is why a rate-limited send is
 retried in place rather than left to the supervisor — a 429 while sending
 used to drop the message outright, with one @[Error]@ naming this worker and
 nothing naming what was lost.
-}
processActionsIndefinitely ::
    MonadLogger m =>
    MonadUnliftIO m =>
    BotApp model action ->
    BotEnv model action ->
    m I.Thread
processActionsIndefinitely botApp botEnv =
    I.worker "TelegramBotSimple.processActionsIndefinitely" $ const $ do
        logWarnIO <- askLogWarnIO
        liftIO (runClient logWarnIO)
  where
    runClient logWarnIO =
        runClientWithException (processActionJob botApp botEnv logWarnIO) (botClientEnv botEnv)

{- | Capture the caller's logging context as a plain 'IO' action.

 The bot loops run in 'ClientM', which has no 'MonadLogger' instance and
 cannot be given one without wrapping every Telegram call. Rather than thread
 a logger type through the API, callers hand down the one thing those loops
 need: somewhere to put a warning. Call this from inside the worker body so
 the captured context is the worker's own.
-}
askLogWarnIO :: (MonadLogger m, MonadUnliftIO m) => m (Text -> IO ())
askLogWarnIO = do
    runInIO <- askRunInIO
    pure (runInIO . logWarnN)

{- | Sleep out a flood-control wait that Telegram has asked us for.

 Every wait is announced, not just the long ones. Before this existed a 429
 produced an @[Error]@ line naming the dead worker, so staying silent here
 would trade a misleading log entry for no log entry — and rate limiting would
 become invisible rather than merely confusing. They are rare enough
 (twenty-two in three days of prod, all in two bursts) that logging each one
 costs nothing.
-}
waitOutFloodControl :: (Text -> IO ()) -> Text -> Telegram.Seconds -> IO ()
waitOutFloodControl logWarnIO request (Telegram.Seconds seconds) = do
    logWarnIO $
        "Telegram rate-limited "
            <> request
            <> "; waiting "
            <> pack (show seconds)
            <> "s before retrying"
    threadDelay (sec (fromIntegral seconds))

-- | Start 'Telegram.Update' polling for a bot.
startBotPolling ::
    forall (unit :: Rat) model action.
    (KnownDivRat unit Microsecond) =>
    Time unit ->
    (Text -> IO ()) ->
    BotApp model action ->
    BotEnv model action ->
    ClientM ()
startBotPolling period logWarnIO BotApp{..} botEnv@BotEnv{..} =
    startPolling period logWarnIO handleUpdate
  where
    handleUpdate update = void . liftIO . forkIO $ do
        maction <- botAction update <$> readTVarIO botModelVar
        forM_ maction (issueAction botEnv (Just update))

{- | Start 'Telegram.Update' polling with a given update handler.

 Takes a warning sink because 'ClientM' has no 'MonadLogger' instance, and the
 one thing worse than a rate-limited bot is one that has silently gone to
 sleep for a minute with nothing in the journal to say so.
-}
startPolling ::
    forall (unit :: Rat).
    (KnownDivRat unit Microsecond) =>
    Time unit ->
    (Text -> IO ()) ->
    (Telegram.Update -> ClientM ()) ->
    ClientM ()
startPolling period logWarnIO handleUpdate = go Nothing
  where
    go lastUpdateId = do
        let inc (Telegram.UpdateId n) = Telegram.UpdateId (n + 1)
            offset = fmap inc lastUpdateId
        res <- getUpdates offset
        nextUpdateId <- do
            let updates = Telegram.responseResult res
                updateIds = map Telegram.updateUpdateId updates
                maxUpdateId = maximum (Nothing : map Just updateIds)
            mapM_ handleUpdate updates
            pure maxUpdateId
        liftIO $ threadDelay period
        go nextUpdateId

    -- Retries a rate-limited poll forever, and with the SAME offset.
    --
    -- Forever, because a bot that stops polling is a dead bot with a live
    -- process, which is worse than a slow one. The wait is bounded per attempt
    -- and re-read from Telegram each time, so this cannot spin.
    --
    -- The offset is the reason this retry lives here rather than in the
    -- supervisor. Updates are confirmed by the NEXT poll's offset, so anything
    -- handled in the call that got rate-limited is still unconfirmed; letting
    -- the thread die restarts 'go' at 'Nothing' and Telegram re-delivers them,
    -- running those commands a second time. Retrying in place keeps the offset
    -- we already had.
    getUpdates offset =
        Telegram.getUpdates (Telegram.GetUpdatesRequest offset Nothing Nothing Nothing)
            `catchError` \err -> case retryAfterFromClientError err of
                Nothing -> throwError err
                Just retryAfter -> do
                    liftIO $ waitOutFloodControl logWarnIO "getUpdates" retryAfter
                    getUpdates offset
