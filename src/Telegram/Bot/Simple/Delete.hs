module Telegram.Bot.Simple.Delete where

import           Control.Monad.Reader

import           Telegram.Bot.API (MessageId)
import           Telegram.Bot.Simple.Eff
import           Telegram.Bot.Simple.Reply

import qualified Telegram.Bot.API     as Telegram

-- | Delete message in the current chat (if possible).
deleteMessage :: MessageId -> BotM ()
deleteMessage msgId = do
  mchatId <- currentChatId
  case mchatId of
    Just chatId -> void $ liftClientM $ Telegram.deleteMessage chatId msgId
    Nothing     -> liftIO $ putStrLn "No chat to delete message"
