{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Telegram.Bot.API.Methods where

import           Data.Aeson
import           Data.Proxy
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Servant.API
import           Servant.Client                  hiding (Response)

import           Telegram.Bot.API.Internal.Utils
import           Telegram.Bot.API.MakingRequests
import           Telegram.Bot.API.Types

-- * Available methods

-- ** 'getMe'

type GetMe = "getMe" :> Get '[JSON] (Response User)

-- | A simple method for testing your bot's auth token.
-- Requires no parameters.
-- Returns basic information about the bot in form of a 'User' object.
getMe :: ClientM (Response User)
getMe = client (Proxy @GetMe)

-- ** 'getChatAdministrators'

type GetChatAdministrators = "getChatAdministrators"
    :> RequiredQueryParam "chat_id" ChatId
    :> Get '[JSON] (Response [ChatMember])

-- | Use this method to get list of chat administrators
getChatAdministrators :: ChatId -> ClientM (Response [ChatMember])
getChatAdministrators = client (Proxy @GetChatAdministrators)

-- ** 'deleteMessage'

-- | Notice that deleting by POST method was bugged, so we use GET
type DeleteMessage = "deleteMessage"
  :> RequiredQueryParam "chat_id" ChatId
  :> RequiredQueryParam "message_id" MessageId
  :> Get '[JSON] (Response Bool)

-- | Use this method to delete message in chat.
-- On success, the sent Bool is returned.
deleteMessage :: ChatId -> MessageId -> ClientM (Response Bool)
deleteMessage = client (Proxy @DeleteMessage)

-- ** 'sendMessage'

type SendMessage
  = "sendMessage" :> ReqBody '[JSON] SendMessageRequest :> Post '[JSON] (Response Message)

-- | Use this method to send text messages.
-- On success, the sent 'Message' is returned.
sendMessage :: SendMessageRequest -> ClientM (Response Message)
sendMessage = client (Proxy @SendMessage)

-- ** 'forwardMessage'

type ForwardMessage
  = "forwardMessage" :> ReqBody '[JSON] ForwardMessageRequest :> Post '[JSON] (Response Message)

-- | Use this method to forward messages of any kind.
-- On success, the sent Message is returned.
forwardMessage :: ForwardMessageRequest -> ClientM (Response Message)
forwardMessage = client (Proxy @ForwardMessage)

-- | Unique identifier for the target chat
-- or username of the target channel (in the format @\@channelusername@).
data SomeChatId
  = SomeChatId ChatId       -- ^ Unique chat ID.
  | SomeChatUsername Text   -- ^ Username of the target channel.
  deriving (Show, Generic)

instance ToJSON   SomeChatId where toJSON = genericSomeToJSON
instance FromJSON SomeChatId where parseJSON = genericSomeParseJSON

-- | Additional interface options.
-- A JSON-serialized object for an inline keyboard, custom reply keyboard,
-- instructions to remove reply keyboard or to force a reply from the user.
data SomeReplyMarkup
  = SomeInlineKeyboardMarkup InlineKeyboardMarkup
  | SomeReplyKeyboardMarkup  ReplyKeyboardMarkup
  | SomeReplyKeyboardRemove  ReplyKeyboardRemove
  | SomeForceReply           ForceReply
  deriving (Show, Generic)

instance ToJSON   SomeReplyMarkup where toJSON = genericSomeToJSON
instance FromJSON SomeReplyMarkup where parseJSON = genericSomeParseJSON

data ParseMode
  = Markdown
  | HTML
  deriving (Show, Generic)

instance ToJSON   ParseMode
instance FromJSON ParseMode

-- | Request parameters for 'sendMessage'.
data SendMessageRequest = SendMessageRequest
  { sendMessageChatId                :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , sendMessageText                  :: Text -- ^ Text of the message to be sent.
  , sendMessageParseMode             :: Maybe ParseMode -- ^ Send 'Markdown' or 'HTML', if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendMessageDisableWebPagePreview :: Maybe Bool -- ^ Disables link previews for links in this message.
  , sendMessageDisableNotification   :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendMessageReplyToMessageId      :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , sendMessageReplyMarkup           :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  } deriving (Generic)

instance ToJSON   SendMessageRequest where toJSON = gtoJSON
instance FromJSON SendMessageRequest where parseJSON = gparseJSON

data ForwardMessageRequest = ForwardMessageRequest
  { forwardMessageChatId              :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , forwardMessageFromChatId          :: SomeChatId -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @channelusername)
  , forwardMessageDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , forwardMessageMessageId           :: MessageId -- ^ Message identifier in the chat specified in from_chat_id
  } deriving (Generic)

instance ToJSON   ForwardMessageRequest where toJSON = gtoJSON
instance FromJSON ForwardMessageRequest where parseJSON = gparseJSON
