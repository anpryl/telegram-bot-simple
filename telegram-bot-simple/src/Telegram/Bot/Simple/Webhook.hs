{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
module Telegram.Bot.Simple.Webhook (webhookApp) where

import           Control.Concurrent                  (forkIO)
import           Control.Concurrent.STM
import           Control.Monad                       (forM_, void)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Servant

import           Telegram.Bot.API.GettingUpdates     (Update)
import           Telegram.Bot.Simple.BotApp.Internal

type WebhookAPI = ReqBody '[JSON] Update :> Post '[JSON] ()

server :: BotApp model action -> BotEnv model action -> Server WebhookAPI
server BotApp {..} botEnv@BotEnv {..} =
  updateHandler
  where
    updateHandler :: Update -> Handler ()
    updateHandler update = liftIO $ handleUpdate update
    handleUpdate update = void . forkIO $ do
      maction <- botAction update <$> readTVarIO botModelVar
      forM_ maction (\act -> issueAction botEnv (Just update) (Just act))

webhookAPI :: Proxy WebhookAPI
webhookAPI = Proxy

app :: BotApp model action -> BotEnv model action -> Application
app botApp botEnv = serve webhookAPI $ server botApp botEnv

webhookApp :: BotApp model action -> BotEnv model action -> Application
webhookApp = app

