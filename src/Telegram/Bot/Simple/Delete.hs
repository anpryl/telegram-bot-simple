module Telegram.Bot.Simple.Delete where

import Control.Monad.Reader
import Telegram.Bot.API (ChatId, MessageId, messageMessageId)
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.Eff
import Telegram.Bot.Simple.Reply

-- | Delete message from chat
deleteChatMessage :: ChatId -> MessageId -> BotM ()
deleteChatMessage chatId = void . liftClientM . Telegram.deleteMessage chatId

deleteChatMessageResponse :: ChatId -> MessageId -> BotM (Telegram.Response Bool)
deleteChatMessageResponse chatId = liftClientM . Telegram.deleteMessage chatId

-- | Delete message in the current chat (if possible).
deleteMessage :: MessageId -> BotM ()
deleteMessage msgId = do
    mchatId <- currentChatId
    case mchatId of
        Just chatId -> deleteChatMessage chatId msgId
        Nothing -> liftIO $ putStrLn "No chat to delete message"

-- | Delete current update message from current chat (if possible).
deleteUpdateMessage :: BotM ()
deleteUpdateMessage = do
    mupdate <- asks botContextUpdate
    let mmsgId = fmap messageMessageId . Telegram.extractUpdateMessage =<< mupdate
    case mmsgId of
        Just msgId -> deleteMessage msgId
        Nothing -> liftIO $ putStrLn "No message to delete"
