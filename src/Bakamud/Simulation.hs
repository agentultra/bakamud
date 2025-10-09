{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Simulation where

import Bakamud.Server
import Bakamud.Server.Client
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class

simulation :: MonadIO m => BakamudServer m ()
simulation = do
  broadcast "Hello"
  liftIO $ threadDelay 3000000
  simulation
