{-# LANGUAGE OverloadedStrings #-}

module Bakamud.Server where

import Bakamud.Network.Connection
import Bakamud.Server.Client
import Bakamud.Server.Command
import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader

nextConnectionId :: MonadIO m => BakamudServer m ConnectionId
nextConnectionId = do
  connectionLimit <- asks _serverStateConnectionLimit
  connectionNextId <- asks _serverStateConnectionNextId
  liftIO . atomically . stateTVar connectionNextId $ \currentId -> do
    let nextId = if currentId == connectionLimit then 0 else currentId + 1
    (ConnectionId $ fromIntegral nextId, nextId)

dispatchCommand :: MonadIO m => (ConnectionId, Command) -> BakamudServer m ()
dispatchCommand (connectionId, command) =
  case command of
    Motd -> put connectionId "THIS IS THE MOTD"
    _ -> liftIO $ print (connectionId, command)
