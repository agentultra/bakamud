{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Simulation where

import Data.Foldable (traverse_)
import Bakamud.Network.Connection
import Bakamud.Server
import Bakamud.Server.State
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader

simulation :: MonadIO m => BakamudServer m ()
simulation = do
  state <- ask
  serverConnections <- liftIO . atomically . readTVar $ _serverStateConnections state
  liftIO $ (`traverse_` serverConnections) $ \Connection {..} ->
    atomically $ writeTBQueue _connectionOutput "Hello!\n"
  liftIO $ threadDelay 3000000
  simulation
