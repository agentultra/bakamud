{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Simulation where

import Bakamud.Server.Monad
import Bakamud.Server.Client
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class

simulation :: MonadIO m => BakamudServer m ()
simulation = do
  broadcast "Hello"
  liftIO $ threadDelay 3000000
  simulation
