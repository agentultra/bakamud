{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Simulation where

import Bakamud.Server
import Bakamud.Server.State
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader

simulation :: MonadIO m => BakamudServer m ()
simulation = do
  bchan <- asks _serverStateBroadcastChannel
  liftIO . atomically $ writeTChan bchan "Hello!"
  liftIO $ threadDelay 3000000
  simulation
