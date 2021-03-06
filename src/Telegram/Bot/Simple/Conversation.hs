{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Telegram.Bot.Simple.Conversation where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.BotApp
import Telegram.Bot.Simple.Eff

{- | Make bot to have a separate state for each conversation.

 Common use (to have a separate state for each chat):

 @
 'conversationBot' 'Telegram.updateChatId' bot
 @
-}
conversationBot ::
    (Eq conversation, Hashable conversation) =>
    -- | How to disambiguate conversations.
    (Telegram.Update -> Maybe conversation) ->
    BotApp model action ->
    BotApp (HashMap (Maybe conversation) model) (Maybe conversation, action)
conversationBot toConversation BotApp{..} =
    BotApp
        { botInitialModel = conversationInitialModel
        , botAction = conversationAction
        , botHandler = conversationHandler
        , botJobs = conversationJobs
        , botErrorHandlers = conversationErrorHandlers
        , botForkErrorHandler = Nothing
        }
  where
    conversationErrorHandlers = toConversationErrorHandler <$> botErrorHandlers
    toConversationErrorHandler (Handler handler) =
        Handler $ ((,) <$> asks (toConversation <=< botContextUpdate) <*>) . handler
    conversationInitialModel = HashMap.empty
    conversationAction update conversations = do
        conversation <- toConversation update
        let model = fromMaybe botInitialModel (HashMap.lookup (Just conversation) conversations)
        (Just conversation,) <$> botAction update model
    conversationHandler (conversation, action) conversations =
        bimap (conversation,) (\m -> HashMap.insert conversation m conversations) $
            botHandler action model
      where
        model = fromMaybe botInitialModel (HashMap.lookup conversation conversations)
    conversationJobs = map toConversationJob botJobs
    toConversationJob BotJob{..} =
        BotJob
            { botJobSchedule = botJobSchedule
            , botJobTask = HashMap.traverseWithKey $
                \conversation -> first (conversation,) . botJobTask
            }

{- | Pass latest 'Telegram.Update' to all bot jobs.

 This enables jobs to easily send notifications.
-}
useLatestUpdateInJobs ::
    BotApp model action ->
    BotApp (Maybe Telegram.Update, model) (Maybe Telegram.Update, action)
useLatestUpdateInJobs BotApp{..} =
    BotApp
        { botInitialModel = (Nothing, botInitialModel)
        , botAction = newAction
        , botHandler = newHandler
        , botJobs = newJobs
        , botErrorHandlers = newErrorHandlers
        , botForkErrorHandler = Nothing
        }
  where
    newErrorHandlers = toNewErrorHandler <$> botErrorHandlers
    toNewErrorHandler (Handler handler) =
        Handler $ ((,) <$> asks botContextUpdate <*>) . handler
    newAction update (_, model) = (Just update,) <$> botAction update model
    newHandler (update, action) (_update, model) =
        bimap (update,) (update,) $
            -- re-enforcing update here is needed for actions issued in jobs
            setEffUpdate update (botHandler action model)
    newJobs = map addUpdateToJob botJobs
    addUpdateToJob BotJob{..} =
        BotJob
            { botJobSchedule = botJobSchedule
            , botJobTask = \(update, model) ->
                bimap (update,) (update,) (setEffUpdate update (botJobTask model))
            }
