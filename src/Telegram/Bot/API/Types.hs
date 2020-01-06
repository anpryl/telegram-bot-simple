{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Telegram.Bot.API.Types where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.String
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import Servant.API
import Telegram.Bot.API.Internal.Utils

type RequiredQueryParam = QueryParam' '[Required, Strict]

newtype Seconds = Seconds Int32
  deriving (Eq, Show, Num, ToJSON, FromJSON)

-- * Available types

-- ** User

-- | This object represents a Telegram user or bot.
--
-- <https://core.telegram.org/bots/api#user>
data User
  = User
      { -- | Unique identifier for this user or bot.
        userId :: UserId,
        -- | 'True', if this user is a bot.
        userIsBot :: Bool,
        -- | User's or bot's first name.
        userFirstName :: Text,
        -- | User‘s or bot’s last name
        userLastName :: Maybe Text,
        -- | User‘s or bot’s username
        userUsername :: Maybe Text,
        -- | IETF language tag of the user's language
        userLanguageCode :: Maybe Text
      }
  deriving (Show, Generic)

-- | Unique identifier for this user or bot.
newtype UserId = UserId Int32
  deriving (Eq, Show, ToJSON, FromJSON)

instance ToHttpApiData UserId where toUrlPiece = pack . show @Int32 . coerce

-- ** Chat

-- | This object represents a chat.
--
-- <https://core.telegram.org/bots/api#chat>
data Chat
  = Chat
      { -- | Unique identifier for this chat. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
        chatId :: ChatId,
        -- | Type of chat.
        chatType :: ChatType,
        -- | Title, for supergroups, channels and group chats
        chatTitle :: Maybe Text,
        -- | Username, for private chats, supergroups and channels if available
        chatUsername :: Maybe Text,
        -- | First name of the other party in a private chat
        chatFirstName :: Maybe Text,
        -- | Last name of the other party in a private chat
        chatLastName :: Maybe Text,
        -- | 'True' if a group has ‘All Members Are Admins’ enabled.
        chatAllMembersAreAdministrators :: Maybe Bool,
        -- | Chat photo. Returned only in getChat.
        chatPhoto :: Maybe ChatPhoto,
        -- | Description, for supergroups and channel chats. Returned only in getChat.
        chatDescription :: Maybe Text,
        -- | Chat invite link, for supergroups and channel chats. Returned only in getChat.
        chatInviteLink :: Maybe Text,
        -- | Pinned message, for supergroups. Returned only in getChat.
        chatPinnedMessage :: Maybe Message,
        -- | For supergroups, name of group sticker set. Returned only in getChat.
        chatStickerSetName :: Maybe Text,
        -- | True, if the bot can change the group sticker set. Returned only in getChat.
        chatCanSetStickerSet :: Maybe Bool
      }
  deriving (Generic, Show)

-- | Unique identifier for this chat.
newtype ChatId = ChatId Integer
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

instance ToHttpApiData ChatId where toUrlPiece a = pack . show @Integer $ coerce a

-- | Type of chat.
data ChatType
  = ChatTypePrivate
  | ChatTypeGroup
  | ChatTypeSupergroup
  | ChatTypeChannel
  deriving (Generic, Show, Eq)

instance ToJSON ChatType where toJSON = gtoJSON

instance FromJSON ChatType where parseJSON = gparseJSON

-- ** Message

-- | This object represents a message.
data Message
  = Message
      { -- | Unique message identifier inside this chat
        messageMessageId :: MessageId,
        -- | Sender, empty for messages sent to channels
        messageFrom :: Maybe User,
        -- | Date the message was sent in Unix time
        messageDate :: POSIXTime,
        -- | Conversation the message belongs to
        messageChat :: Chat,
        -- | For forwarded messages, sender of the original message
        messageForwardFrom :: Maybe User,
        -- | For messages forwarded from channels, information about the original channel
        messageForwardFromChat :: Maybe Chat,
        -- | For messages forwarded from channels, identifier of the original message in the channel
        messageForwardFromMessageId :: Maybe MessageId,
        -- | For messages forwarded from channels, signature of the post author if present
        messageForwardSignature :: Maybe Text,
        -- | For forwarded messages, date the original message was sent in Unix time
        messageForwardDate :: Maybe POSIXTime,
        -- | For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
        messageReplyToMessage :: Maybe Message,
        -- | Date the message was last edited in Unix time
        messageEditDate :: Maybe POSIXTime,
        -- | The unique identifier of a media message group this message belongs to
        messageMediaGroupId :: Maybe MediaGroupId,
        -- | Signature of the post author for messages in channels
        messageAuthorSignature :: Maybe Text,
        -- | For text messages, the actual UTF-8 text of the message, 0-4096 characters.
        messageText :: Maybe Text,
        -- | For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
        messageEntities :: Maybe [MessageEntity],
        -- | For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption
        messageCaptionEntities :: Maybe [MessageEntity],
        -- | Message is an audio file, information about the file
        messageAudio :: Maybe Audio,
        -- | Message is a general file, information about the file
        messageDocument :: Maybe Document,
        --  , messageGame :: Maybe Game -- ^ Message is a game, information about the game. More about games »

        -- | Message is a photo, available sizes of the photo
        messagePhoto :: Maybe [PhotoSize],
        --  , messageSticker :: Maybe Sticker -- ^ Message is a sticker, information about the sticker

        -- | Message is a video, information about the video
        messageVideo :: Maybe Video,
        -- | Message is a voice message, information about the file
        messageVoice :: Maybe Voice,
        -- | Message is a video note, information about the video message
        messageVideoNote :: Maybe VideoNote,
        -- | Caption for the audio, document, photo, video or voice, 0-200 characters
        messageCaption :: Maybe Text,
        -- | Message is a shared contact, information about the contact
        messageContact :: Maybe Contact,
        -- | Message is a shared location, information about the location
        messageLocation :: Maybe Location,
        -- | Message is a venue, information about the venue
        messageVenue :: Maybe Venue,
        -- | New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)
        messageNewChatMembers :: Maybe [User],
        -- | A member was removed from the group, information about them (this member may be the bot itself)
        messageLeftChatMember :: Maybe User,
        -- | A chat title was changed to this value
        messageNewChatTitle :: Maybe Text,
        -- | A chat photo was change to this value
        messageNewChatPhoto :: Maybe [PhotoSize],
        -- | Service message: the chat photo was deleted
        messageDeleteChatPhoto :: Maybe Bool,
        -- | Service message: the group has been created
        messageGroupChatCreated :: Maybe Bool,
        -- | Service message: the supergroup has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a supergroup when it is created. It can only be found in reply_to_message if someone replies to a very first message in a directly created supergroup.
        messageSupergroupChatCreated :: Maybe Bool,
        -- | Service message: the channel has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a channel when it is created. It can only be found in reply_to_message if someone replies to a very first message in a channel.
        messageChannelChatCreated :: Maybe Bool,
        -- | The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
        messageMigrateToChatId :: Maybe ChatId,
        -- | The supergroup has been migrated from a group with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
        messageMigrateFromChatId :: Maybe ChatId,
        -- | Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply.
        messagePinnedMessage :: Maybe Message
      }
  --  , messageInvoice :: Maybe Invoice -- ^ Message is an invoice for a payment, information about the invoice. More about payments »
  --  , messageSuccessfulPayment :: Maybe SuccessfulPayment -- ^ Message is a service message about a successful payment, information about the payment. More about payments »
  deriving (Generic, Show)

-- | Unique message identifier inside this chat.
newtype MessageId = MessageId Int32
  deriving (Eq, Show, ToJSON, FromJSON)

instance ToHttpApiData MessageId where toUrlPiece a = pack . show @Int32 $ coerce a

-- | The unique identifier of a media message group a message belongs to.
newtype MediaGroupId = MediaGroupId Text
  deriving (Eq, Show, ToJSON, FromJSON)

-- ** MessageEntity

-- | This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.
data MessageEntity
  = MessageEntity
      { -- | Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)
        messageEntityType :: MessageEntityType,
        -- | Offset in UTF-16 code units to the start of the entity
        messageEntityOffset :: Int32,
        -- | Length of the entity in UTF-16 code units
        messageEntityLength :: Int32,
        -- | For “text_link” only, url that will be opened after user taps on the text
        messageEntityUrl :: Maybe Text,
        -- | For “text_mention” only, the mentioned user
        messageEntityUser :: Maybe User
      }
  deriving (Generic, Show)

-- | Type of the entity. Can be mention (@username), hashtag, cashtag, bot_command, url, email, phone_number, bold (bold text), italic (italic text), code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)
data MessageEntityType
  = MessageEntityMention
  | MessageEntityHashtag
  | MessageEntityBotCommand
  | MessageEntityUrl
  | MessageEntityEmail
  | MessageEntityBold
  | MessageEntityItalic
  | MessageEntityCode
  | MessageEntityPre
  | MessageEntityTextLink
  | MessageEntityTextMention
  | MessageEntityPhoneNumber
  | MessageEntityCashtag
  deriving (Eq, Show, Generic)

instance ToJSON MessageEntityType where toJSON = gtoJSON

instance FromJSON MessageEntityType where parseJSON = gparseJSON

-- ** 'PhotoSize'

-- | This object represents one size of a photo or a file / sticker thumbnail.
data PhotoSize
  = PhotoSize
      { -- | Unique identifier for this file
        photoSizeFileId :: FileId,
        -- | Photo width
        photoSizeWidth :: Int32,
        -- | Photo height
        photoSizeHeight :: Int32,
        -- | File size
        photoSizeFileSize :: Maybe Int32
      }
  deriving (Generic, Show)

-- | Unique identifier for this file.
newtype FileId = FileId Text
  deriving (Eq, Show, ToJSON, FromJSON)

-- ** 'Audio'

-- | This object represents an audio file to be treated as music by the Telegram clients.
data Audio
  = Audio
      { -- | Unique identifier for this file
        audioFileId :: FileId,
        -- | Duration of the audio in seconds as defined by sender
        audioDuration :: Seconds,
        -- | Performer of the audio as defined by sender or by audio tags
        audioPerformer :: Maybe Text,
        -- | Title of the audio as defined by sender or by audio tags
        audioTitle :: Maybe Text,
        -- | MIME type of the file as defined by sender
        audioMimeType :: Maybe Text,
        -- | File size
        audioFileSize :: Maybe Int32
      }
  deriving (Generic, Show)

-- ** 'Document'

-- | This object represents a general file (as opposed to photos, voice messages and audio files).
data Document
  = Document
      { -- | Unique file identifier
        documentFileId :: FileId,
        -- | Document thumbnail as defined by sender
        documentThumb :: Maybe PhotoSize,
        -- | Original filename as defined by sender
        documentFileName :: Maybe Text,
        -- | MIME type of the file as defined by sender
        documentMimeType :: Maybe Text,
        -- | File size
        documentFileSize :: Maybe Int32
      }
  deriving (Generic, Show)

-- ** 'Video'

-- | This object represents a video file.
data Video
  = Video
      { -- | Unique identifier for this file
        videoFileId :: FileId,
        -- | Video width as defined by sender
        videoWidth :: Int32,
        -- | Video height as defined by sender
        videoHeight :: Int32,
        -- | Duration of the video in seconds as defined by sender
        videoDuration :: Seconds,
        -- | Video thumbnail
        videoThumb :: Maybe PhotoSize,
        -- | Mime type of a file as defined by sender
        videoMimeType :: Maybe Text,
        -- | File size
        videoFileSize :: Maybe Int32
      }
  deriving (Generic, Show)

-- ** 'Voice'

-- | This object represents a voice note.
data Voice
  = Voice
      { -- | Unique identifier for this file
        voiceFileId :: FileId,
        -- | Duration of the audio in seconds as defined by sender
        voiceDuration :: Seconds,
        -- | MIME type of the file as defined by sender
        voiceMimeType :: Maybe Text,
        -- | File size
        voiceFileSize :: Maybe Int32
      }
  deriving (Generic, Show)

-- ** 'VideoNote'

-- | This object represents a video message (available in Telegram apps as of v.4.0).
data VideoNote
  = VideoNote
      { -- | Unique identifier for this file
        videoNoteFileId :: Text,
        -- | Video width and height as defined by sender
        videoNoteLength :: Int32,
        -- | Duration of the video in seconds as defined by sender
        videoNoteDuration :: Seconds,
        -- | Video thumbnail
        videoNoteThumb :: Maybe PhotoSize,
        -- | File size
        videoNoteFileSize :: Maybe Int32
      }
  deriving (Generic, Show)

-- ** 'Contact'

-- | This object represents a phone contact.
data Contact
  = Contact
      { -- | Contact's phone number
        contactPhoneNumber :: Text,
        -- | Contact's first name
        contactFirstName :: Text,
        -- | Contact's last name
        contactLastName :: Maybe Text,
        -- | Contact's user identifier in Telegram
        contactUserId :: Maybe UserId
      }
  deriving (Generic, Show)

-- ** Location

-- | This object represents a point on the map.
data Location
  = Location
      { -- | Longitude as defined by sender
        locationLongitude :: Float,
        -- | Latitude as defined by sender
        locationLatitude :: Float
      }
  deriving (Generic, Show)

-- ** 'Venue'

-- | This object represents a venue.
data Venue
  = Venue
      { -- | Venue location
        venueLocation :: Location,
        -- | Name of the venue
        venueTitle :: Text,
        -- | Address of the venue
        venueAddress :: Text,
        -- | Foursquare identifier of the venue
        venueFoursquareId :: Maybe Text
      }
  deriving (Generic, Show)

-- ** 'UserProfilePhotos'

-- | This object represent a user's profile pictures.
data UserProfilePhotos
  = UserProfilePhotos
      { -- | Total number of profile pictures the target user has
        userProfilePhotosTotalCount :: Int32,
        -- | Requested profile pictures (in up to 4 sizes each)
        userProfilePhotosPhotos :: [[PhotoSize]]
      }
  deriving (Generic, Show)

-- ** 'File'

-- | This object represents a file ready to be downloaded.
-- The file can be downloaded via the link @https://api.telegram.org/file/bot<token>/<file_path>@.
-- It is guaranteed that the link will be valid for at least 1 hour.
-- When the link expires, a new one can be requested by calling getFile.
data File
  = File
      { -- | Unique identifier for this file
        fileFileId :: FileId,
        -- | File size, if known
        fileFileSize :: Maybe Int32,
        -- | File path. Use https://api.telegram.org/file/bot<token>/<file_path> to get the file.
        fileFilePath :: Maybe Text
      }
  deriving (Generic, Show)

-- ** 'ReplyKeyboardMarkup'

-- | This object represents a custom keyboard with reply options (see Introduction to bots for details and examples).
data ReplyKeyboardMarkup
  = ReplyKeyboardMarkup
      { -- | Array of button rows, each represented by an Array of KeyboardButton objects
        replyKeyboardMarkupKeyboard :: [[KeyboardButton]],
        -- | Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons). Defaults to false, in which case the custom keyboard is always of the same height as the app's standard keyboard.
        replyKeyboardMarkupResizeKeyboard :: Maybe Bool,
        -- | Requests clients to hide the keyboard as soon as it's been used. The keyboard will still be available, but clients will automatically display the usual letter-keyboard in the chat – the user can press a special button in the input field to see the custom keyboard again. Defaults to false.
        replyKeyboardMarkupOneTimeKeyboard :: Maybe Bool,
        -- | Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
        replyKeyboardMarkupSelective :: Maybe Bool
      }
  deriving (Generic, Show)

-- ** 'KeyboardButton'

-- | This object represents one button of the reply keyboard.
-- For simple text buttons String can be used instead of this object
-- to specify text of the button. Optional fields are mutually exclusive.
data KeyboardButton
  = KeyboardButton
      { -- | Text of the button. If none of the optional fields are used, it will be sent as a message when the button is pressed
        keyboardButtonText :: Text,
        -- | If True, the user's phone number will be sent as a contact when the button is pressed. Available in private chats only
        keyboardButtonRequestContact :: Maybe Bool,
        -- | If True, the user's current location will be sent when the button is pressed. Available in private chats only
        keyboardButtonRequestLocation :: Maybe Bool
      }
  deriving (Generic, Show)

instance IsString KeyboardButton where
  fromString s = KeyboardButton (fromString s) Nothing Nothing

-- ** 'ReplyKeyboardRemove'

-- | Upon receiving a message with this object,
-- Telegram clients will remove the current custom keyboard
-- and display the default letter-keyboard.
--
-- By default, custom keyboards are displayed until a new keyboard is sent by a bot.
-- An exception is made for one-time keyboards that are hidden immediately after
-- the user presses a button (see 'ReplyKeyboardMarkup').
data ReplyKeyboardRemove
  = ReplyKeyboardRemove
      { -- | Requests clients to remove the custom keyboard (user will not be able to summon this keyboard; if you want to hide the keyboard from sight but keep it accessible, use one_time_keyboard in ReplyKeyboardMarkup)
        replyKeyboardRemoveRemoveKeyboard :: Bool,
        -- | Use this parameter if you want to remove the keyboard for specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
        replyKeyboardRemoveSelective :: Maybe Bool
      }
  deriving (Generic, Show)

-- ** 'InlineKeyboardMarkup'

-- | This object represents an inline keyboard that appears
-- right next to the message it belongs to.
data InlineKeyboardMarkup {- HLINT ignore InlineKeyboardMarkup "Use newtype instead of data" -}
  = InlineKeyboardMarkup
      { -- | Array of button rows, each represented by an Array of InlineKeyboardButton objects
        inlineKeyboardMarkupInlineKeyboard :: [[InlineKeyboardButton]]
      }
  deriving (Generic, Show)

-- ** 'InlineKeyboardButton'

-- | This object represents one button of an inline keyboard. You must use exactly one of the optional fields.
data InlineKeyboardButton
  = InlineKeyboardButton
      { -- | Label text on the button
        inlineKeyboardButtonText :: Text,
        -- | HTTP url to be opened when button is pressed
        inlineKeyboardButtonUrl :: Maybe Text,
        -- | Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes
        inlineKeyboardButtonCallbackData :: Maybe Text,
        -- | If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot‘s username and the specified inline query in the input field. Can be empty, in which case just the bot’s username will be inserted.
        inlineKeyboardButtonSwitchInlineQuery :: Maybe Text,
        -- | If set, pressing the button will insert the bot‘s username and the specified inline query in the current chat's input field. Can be empty, in which case only the bot’s username will be inserted.
        inlineKeyboardButtonSwitchInlineQueryCurrentChat :: Maybe Text,
        --  , inlineKeyboardButtonCallbackGame :: Maybe CallbackGame -- ^ Description of the game that will be launched when the user presses the button.

        -- | Specify True, to send a Pay button.
        inlineKeyboardButtonPay :: Maybe Bool
      }
  deriving (Generic, Show)

labeledInlineKeyboardButton :: Text -> InlineKeyboardButton
labeledInlineKeyboardButton label = InlineKeyboardButton label Nothing Nothing Nothing Nothing Nothing

-- ** 'CallbackQuery'

-- | This object represents an incoming callback query from a callback button
-- in an inline keyboard. If the button that originated the query was attached
-- to a message sent by the bot, the field message will be present.
-- If the button was attached to a message sent via the bot (in inline mode),
-- the field @inline_message_id@ will be present.
-- Exactly one of the fields data or game_short_name will be present.
data CallbackQuery
  = CallbackQuery
      { -- | Unique identifier for this query
        callbackQueryId :: CallbackQueryId,
        -- | Sender
        callbackQueryFrom :: User,
        -- | Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old
        callbackQueryMessage :: Maybe Message,
        -- | Identifier of the message sent via the bot in inline mode, that originated the query.
        callbackQueryInlineMessageId :: Maybe MessageId,
        -- | Global identifier, uniquely corresponding to the chat to which the message with the callback button was sent. Useful for high scores in games.
        callbackQueryChatInstance :: Text,
        -- | Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field.
        callbackQueryData :: Maybe Text,
        -- | Short name of a Game to be returned, serves as the unique identifier for the game
        callbackQueryGameShortName :: Maybe Text
      }
  deriving (Generic, Show)

newtype CallbackQueryId = CallbackQueryId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ** 'ForceReply'

-- | Upon receiving a message with this object,
-- Telegram clients will display a reply interface to the user
-- (act as if the user has selected the bot‘s message and tapped ’Reply').
-- This can be extremely useful if you want to create user-friendly
-- step-by-step interfaces without having to sacrifice privacy mode.
data ForceReply
  = ForceReply
      { -- | Shows reply interface to the user, as if they manually selected the bot‘s message and tapped ’Reply'
        forceReplyForceReply :: Bool,
        -- | Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
        forceReplySelective :: Maybe Bool
      }
  deriving (Generic, Show)

-- ** Chat photo

-- | Chat photo. Returned only in getChat.
data ChatPhoto
  = ChatPhoto
      { -- | Unique file identifier of small (160x160) chat photo. This file_id can be used only for photo download.
        chatPhotoSmallFileId :: FileId,
        -- | Unique file identifier of big (640x640) chat photo. This file_id can be used only for photo download.
        chatPhotoBigFileId :: FileId
      }
  deriving (Generic, Show)

-- ** 'ChatMember'

-- | This object contains information about one member of a chat.
data ChatMember
  = ChatMember
      { -- | Information about the user
        chatMemberUser :: User,
        -- | The member's status in the chat. Can be “creator”, “administrator”, “member”, “restricted”, “left” or “kicked”
        chatMemberStatus :: Text,
        -- | Restictred and kicked only. Date when restrictions will be lifted for this user, unix time
        chatMemberUntilDate :: Maybe POSIXTime,
        -- | Administrators only. True, if the bot is allowed to edit administrator privileges of that user
        chatMemberCanBeEdited :: Maybe Bool,
        -- | Administrators only. True, if the administrator can change the chat title, photo and other settings
        chatMemberCanChangeInfo :: Maybe Bool,
        -- | Administrators only. True, if the administrator can post in the channel, channels only
        chatMemberCanPostMessages :: Maybe Bool,
        -- | Administrators only. True, if the administrator can edit messages of other users and can pin messages, channels only
        chatMemberCanEditMessages :: Maybe Bool,
        -- | Administrators only. True, if the administrator can delete messages of other users
        chatMemberCanDeleteMessages :: Maybe Bool,
        -- | Administrators only. True, if the administrator can invite new users to the chat
        chatMemberCanInviteUsers :: Maybe Bool,
        -- | Administrators only. True, if the administrator can restrict, ban or unban chat members
        chatMemberCanRestrictMembers :: Maybe Bool,
        -- | Administrators only. True, if the administrator can pin messages, supergroups only
        chatMemberCanPinMessages :: Maybe Bool,
        -- | Administrators only. True, if the administrator can add new administrators with a subset of his own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by the user)
        chatMemberCanPromoteMembers :: Maybe Bool,
        -- | Restricted only. True, if the user can send text messages, contacts, locations and venues
        chatMemberCanSendMessages :: Maybe Bool,
        -- | Restricted only. True, if the user can send audios, documents, photos, videos, video notes and voice notes, implies can_send_messages
        chatMemberCanSendMediaMessages :: Maybe Bool,
        -- | Restricted only. True, if the user can send animations, games, stickers and use inline bots, implies can_send_media_messages
        chatMemberCanSendOtherMessages :: Maybe Bool,
        -- | Restricted only. True, if user may add web page previews to his messages, implies can_send_media_messages
        chatMemberCanAddWebPagePreviews :: Maybe Bool
      }
  deriving (Generic, Show)

-- ** 'ResponseParameters'

-- | Contains information about why a request was unsuccessful.
data ResponseParameters
  = ResponseParameters
      { -- | The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
        responseParametersMigrateToChatId :: Maybe ChatId,
        -- | In case of exceeding flood control, the number of seconds left to wait before the request can be repeated
        responseParametersRetryAfter :: Maybe Seconds
      }
  deriving (Show, Generic)

deriveJSON' ''User

deriveJSON' ''Chat

deriveJSON' ''Message

deriveJSON' ''MessageEntity

deriveJSON' ''PhotoSize

deriveJSON' ''Audio

deriveJSON' ''Document

deriveJSON' ''Video

deriveJSON' ''Voice

deriveJSON' ''VideoNote

deriveJSON' ''Contact

deriveJSON' ''Location

deriveJSON' ''Venue

deriveJSON' ''UserProfilePhotos

deriveJSON' ''File

deriveJSON' ''ReplyKeyboardMarkup

deriveJSON' ''KeyboardButton

deriveJSON' ''ReplyKeyboardRemove

deriveJSON' ''InlineKeyboardMarkup

deriveJSON' ''InlineKeyboardButton

deriveJSON' ''CallbackQuery

deriveJSON' ''ForceReply

deriveJSON' ''ChatPhoto

deriveJSON' ''ChatMember

deriveJSON' ''ResponseParameters
