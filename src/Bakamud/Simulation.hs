{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Simulation where

import Data.Foldable (traverse_)
import Bakamud.Network.Connection
import Bakamud.Server.State
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)

simulation :: TVar ServerState -> IO ()
simulation serverState = do
  state <- atomically $ readTVar serverState
  (`traverse_` (_serverStateConnections state)) $ \Connection {..} ->
    atomically $ writeTBQueue _connectionOutput "Hello!\n"
  threadDelay 3000000
  simulation serverState
