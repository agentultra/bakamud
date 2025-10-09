module Bakamud.Server.Client where

import Bakamud.Server
import Bakamud.Server.State
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text)

broadcast :: MonadIO m => Text -> BakamudServer m ()
broadcast msg = do
  bchan <- asks _serverStateBroadcastChannel
  liftIO . atomically $ writeTChan bchan msg
