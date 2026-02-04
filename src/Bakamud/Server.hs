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
import Data.Text (Text)
import qualified Focus as Focus
import qualified HsLua.Core as Lua
import qualified HsLua.Marshalling as Lua
import qualified StmContainers.Map as SMap

import qualified Debug.Trace as Debug

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
    TokenList tokens -> handleTokenList connectionId tokens
    _ -> undefined -- TODO: handle the error properly

handleLogin :: MonadIO m => ConnectionId -> Username -> Password -> BakamudServer m ()
handleLogin connectionId user pass = do
  challengeResult <- challenge user pass
  case challengeResult of
    ChallengeFail -> put connectionId "Invalid login\n"
    ChallengeSuccess -> do
      connectionsMap <- asks _serverStateConnections
      liftIO . atomically $ do
        let updateAuthSuccessFocus = Focus.adjust updateAuthSuccess
        SMap.focus updateAuthSuccessFocus connectionId connectionsMap
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

handleTokenList :: MonadIO m => ConnectionId -> [Text] -> BakamudServer m ()
handleTokenList _ tokens = do
  Debug.traceM "handleTokenList start"
  result <- withLuaInterpreterLock $ \lstate -> Lua.runWith @Lua.Exception lstate $ do
    getGlobalResult <- Lua.getglobal "handleCommand"
    Debug.traceM $ "handleTokenList - handleCommandResult: " ++ show getGlobalResult
    pushListResult <- Lua.pushList Lua.pushText tokens
    Debug.traceM $ "handleTokenList - pushListResult: " ++ show pushListResult
    r <- Lua.pcallTrace 1 0
    Debug.traceM $ "handleTokenList - pcallTrace result: " ++ show r
    pure r
  Debug.traceM $ "handleTokenList - result: " ++ show result
  pure ()

lockLuaInterpreter :: MonadIO m => BakamudServer m (Maybe Lua.State)
lockLuaInterpreter = do
  interpreterLockTVar <- asks _serverStateLuaInterpreterLock
  interpreterStateTVar <- asks _serverStateLuaInterpreterState
  liftIO . atomically $ do
    interpreterLock <- readTVar interpreterLockTVar
    case interpreterLock of
      Locked -> pure Nothing
      Unlocked -> do
        writeTVar interpreterLockTVar Locked
        luaInterpreterState <- readTVar interpreterStateTVar
        pure $ Just luaInterpreterState

unlockLuaInterpreter :: MonadIO m => Lua.State -> BakamudServer m LockState
unlockLuaInterpreter luaState = do
  interpreterLockTVar <- asks _serverStateLuaInterpreterLock
  interpreterStateTVar <- asks _serverStateLuaInterpreterState
  liftIO . atomically $ do
    interpreterLock <- readTVar interpreterLockTVar
    case interpreterLock of
      Locked -> do
        writeTVar interpreterStateTVar luaState
        writeTVar interpreterLockTVar Unlocked
        pure Unlocked
      Unlocked -> pure Locked

withLuaInterpreterLock :: (MonadIO m, Show a) => (Lua.State -> IO a) -> BakamudServer m (Maybe a)
withLuaInterpreterLock callback = do
  Debug.traceM "withLuaInterpreterLock - start"
  maybeInterpreterLock <- lockLuaInterpreter
  case maybeInterpreterLock of
    Nothing -> pure Nothing
    Just interpreterState -> do
      returnValue <- liftIO $ callback interpreterState
      _ <- unlockLuaInterpreter interpreterState
      pure $ Just returnValue
