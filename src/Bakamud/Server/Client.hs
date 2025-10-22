{-# LANGUAGE RecordWildCards #-}
module Bakamud.Server.Client where

import Bakamud.Network.Connection
import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as Q
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Data.Text (Text)

broadcast :: MonadIO m => Text -> BakamudServer m ()
broadcast msg = do
  bchan <- asks _serverStateBroadcastChannel
  liftIO . atomically $ writeTChan bchan msg

put :: MonadIO m => ConnectionId -> Text -> BakamudServer m ()
put connectionId msg = do
  connectionsTVar <- asks _serverStateConnections
  liftIO . atomically $ do
    connections <- readTVar connectionsTVar
    case Map.lookup connectionId connections of
      Nothing -> undefined
      Just Connection {..} -> Q.writeTBQueue _connectionOutput msg
