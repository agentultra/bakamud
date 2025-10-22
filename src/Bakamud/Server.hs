{-# LANGUAGE OverloadedStrings #-}

module Bakamud.Server where

import Bakamud.Auth
import Bakamud.Network.Connection
import Bakamud.Server.Client
import Bakamud.Server.Command
import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as Map

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
    Motd -> put connectionId "THIS IS THE MOTD\n"
    Login user pass -> handleLogin connectionId user pass
    _ -> liftIO $ print (connectionId, command)

handleLogin :: MonadIO m => ConnectionId -> Username -> Password -> BakamudServer m ()
handleLogin connectionId user pass = do
  challengeResult <- challenge user pass
  case challengeResult of
    ChallengeFail -> put connectionId "Invalid login\n"
    ChallengeSuccess -> do
      connectionsTVar <- asks _serverStateConnections
      liftIO . atomically . (modifyTVar connectionsTVar) $ \connections ->
        Map.adjust updateAuthSuccess connectionId connections
      put connectionId "Success!\n"
  where
    updateAuthSuccess :: Connection -> Connection
    updateAuthSuccess connection =
      connection { _connectionState = Authenticated }

data ChallengeResult = ChallengeSuccess | ChallengeFail deriving (Eq, Show)

challenge :: MonadIO m => Username -> Password -> BakamudServer m ChallengeResult
challenge _ _ = pure ChallengeSuccess
