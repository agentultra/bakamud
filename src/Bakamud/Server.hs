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
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Focus as Focus
import Foreign.C.String (withCString, withCStringLen)
import qualified Foreign.Ptr as Ptr
import Foreign.Storable (peek)
import qualified Lua as Lua
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
  let numTokens = fromIntegral $ length tokens
  result <- withLuaInterpreterLock $ \lstate -> do
    Debug.traceM $ "handleTokenList - withInterpreterLock"
    liftIO $ do
      handleCommandResult <- withCStringLen "handleCommand" $ \(funName, funNameLen) ->
        Lua.hslua_getglobal lstate funName (fromIntegral funNameLen) Ptr.nullPtr
      Debug.traceM $ "handleTokenList - handleCommandResult: " ++ show handleCommandResult
      case handleCommandResult of
        Lua.LUA_TFUNCTION -> do
          Lua.lua_createtable lstate numTokens numTokens
          (`traverse_` (zip [1..] tokens)) $ \(ix, token) -> pushToken lstate ix token
          callResult <- Lua.lua_pcall lstate (Lua.NumArgs 1) (Lua.NumResults 0) (Lua.StackIndex 0)
          Debug.traceM $ "handleTokenList - lua_pcall: " ++ show callResult
        _ -> Debug.trace "Woop!" $ error "handleTokenList: could not find handleCommand user function"
  Debug.traceM $ "handleTokenList - result: " ++ show result
  pure ()
  where
    pushToken :: Lua.State -> Int -> Text -> IO ()
    pushToken l ix v = do
      let statusCode = Ptr.nullPtr
      withCString (Text.unpack v) $ \cstr -> do
        Lua.lua_pushnumber l (Lua.Number $ fromIntegral ix)
        _ <- Lua.lua_pushstring l cstr
        Lua.hslua_settable l (-3) statusCode
      status <- peek @Lua.StatusCode statusCode
      Debug.traceM $ "status: " ++ show status

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
