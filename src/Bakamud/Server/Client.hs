{-# LANGUAGE RecordWildCards #-}
module Bakamud.Server.Client where

import Bakamud.Network.Connection
import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as Q
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text)
import qualified StmContainers.Map as SMap

broadcast :: MonadIO m => Text -> BakamudServer m ()
broadcast msg = do
  bchan <- asks _serverStateBroadcastChannel
  liftIO . atomically $ writeTChan bchan msg

put :: MonadIO m => ConnectionId -> Text -> BakamudServer m ()
put connectionId msg = do
  connectionsMap <- asks _serverStateConnections
  liftIO . atomically $ do
    maybeConnection <- SMap.lookup connectionId connectionsMap
    case maybeConnection of
      Nothing -> undefined -- TODO: should log and continue
      Just Connection {..} -> Q.writeTBQueue _connectionOutput msg
