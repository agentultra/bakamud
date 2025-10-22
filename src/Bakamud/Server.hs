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
import Data.Map.Strict (Map)
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
    Register user pass -> handleRegister connectionId user pass
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
challenge username password = do
  accountsTVar <- asks _serverStateAccounts
  liftIO . atomically $ do
    accounts <- readTVar accountsTVar
    case Map.lookup username accounts of
      Nothing -> pure ChallengeFail
      Just p  ->
        if p == password
        then pure ChallengeSuccess
        else pure ChallengeFail

data RegistrationResult
  = RegistrationSucceeded
  | RegistrationFailed
  deriving (Eq, Show)

handleRegister
  :: MonadIO m
  => ConnectionId
  -> Username
  -> Password
  -> BakamudServer m ()
handleRegister connectionId user pass = do
  accountsTVar <- asks _serverStateAccounts
  result <- liftIO . atomically $ do
    accounts <- readTVar accountsTVar
    case Map.lookup user accounts of
      Just _ -> pure RegistrationFailed
      Nothing -> do
        writeTVar accountsTVar $ addAccount user pass accounts
        pure RegistrationSucceeded
  case result of
    RegistrationFailed -> put connectionId "Username already in use, try another one.\n"
    RegistrationSucceeded -> put connectionId "Registration succeeded!\n"
  where
    addAccount :: Username -> Password -> Map Username Password -> Map Username Password
    addAccount username password accounts =
      Map.insert username password accounts
