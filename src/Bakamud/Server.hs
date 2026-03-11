{-# LANGUAGE OverloadedStrings #-}

module Bakamud.Server where

import Bakamud.Auth
import Bakamud.Account
import Bakamud.Network.Connection
import Bakamud.Server.Client
import Bakamud.Server.Command
import Bakamud.Server.Monad
import Bakamud.Server.State
import Crypto.BCrypt as BCrypt
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Database.SQLite.Simple (NamedParam (..))
import qualified Database.SQLite.Simple as DB
import qualified Focus as Focus
import qualified HsLua.Core as Lua
import qualified HsLua.Marshalling as Lua
import qualified StmContainers.Map as SMap

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
    HandleParseError _ -> pure ()

handleLogin :: MonadIO m => ConnectionId -> Username -> Password -> BakamudServer m ()
handleLogin connectionId username pass = do
  challengeResult <- challenge username pass
  connectionsMap <- asks _serverStateConnections

  case challengeResult of
    ChallengeFail -> put connectionId "Invalid login\n"
    ChallengeSuccess account -> do
      liftIO . atomically $ do
        let updateAuthSuccessFocus = Focus.adjust (updateAuthState account)
        SMap.focus updateAuthSuccessFocus connectionId connectionsMap
      result <- withLuaInterpreterLock $ \lstate -> Lua.runWith @Lua.Exception lstate $ do
            _ <- Lua.getfield (-1) "onLogin"
            _ <- Lua.pushinteger
              . Lua.Integer
              . fromIntegral
              . getConnectionId
              $ connectionId
            r <- Lua.pcallTrace 1 0
            pure r
      when (result /= Just Lua.OK) $ do
        error "Lua code failed"
  where
    updateAuthState :: Account -> Connection -> Connection
    updateAuthState account connection =
      connection { _connectionState = Authenticated
                 , _connectionUserId = Just $ _accountId account
                 }

data ChallengeResult = ChallengeSuccess Account | ChallengeFail deriving (Eq, Show)

challenge :: MonadIO m => Username -> Password -> BakamudServer m ChallengeResult
challenge username (Password password) = do
  --accountsTVar <- asks _serverStateAccounts
  dbHandle <- asks _serverStateDbHandle

  conn <- liftIO . atomically $ do
    readTVar dbHandle
  account <- liftIO $ getAccount conn username
  if BCrypt.validatePassword (Text.encodeUtf8 . getPassword . _accountUserPass $ account) (Text.encodeUtf8 password)
    then pure $ ChallengeSuccess account
    else pure $ ChallengeFail
  -- liftIO . atomically $ do
  --   accounts <- readTVar accountsTVar
  --   case Map.lookup username accounts of
  --     Nothing -> pure ChallengeFail
  --     Just p  ->
  --       if p == password
  --       then pure $ ChallengeSuccess user
  --       else pure ChallengeFail
  where
    getAccount :: DB.Connection -> Username -> IO Account
    getAccount conn (Username uname) = do
      maybeAccount <- listToMaybe <$> DB.queryNamed conn "SELECT * FROM accounts WHERE username = :username" [":username" := uname]
      case maybeAccount of
        Nothing -> error "No such user exists"
        Just account -> pure account

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
  -- accountsTVar <- asks _serverStateAccounts
  dbHandle <- asks _serverStateDbHandle

  conn <- liftIO . atomically $ do
    readTVar dbHandle
  maybeAccount <- liftIO $ getAccount conn user

  case maybeAccount of
    Just _ -> put connectionId "Username already in use, try another one.\n"
    Nothing -> do
      liftIO $ do
        hashedPass <- hashPass pass
        DB.executeNamed conn "INSERT INTO accounts (username, password) VALUES (:username, :password)" [":username" := user, ":password" := hashedPass]
      put connectionId "Registration accepted!\n"

  -- result <- liftIO . atomically $ do
  --   accounts <- readTVar accountsTVar
  --   case Map.lookup user accounts of
  --     Just _ -> pure RegistrationFailed
  --     Nothing -> do
  --       writeTVar accountsTVar $ addAccount user pass accounts
  --       pure RegistrationSucceeded
  -- case result of
  --   RegistrationFailed -> put connectionId "Username already in use, try another one.\n"
  --   RegistrationSucceeded -> put connectionId "Registration succeeded!\n"
  -- where
  --   addAccount :: Username -> Password -> Map Username Password -> Map Username Password
  --   addAccount username password accounts =
  --     Map.insert username password accounts
  where
    hashPass :: Password -> IO Password
    hashPass (Password p) = do
      maybeHashed <- BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy $ Text.encodeUtf8 p
      case maybeHashed of
        Nothing -> error "Could not hash password"
        Just hashed -> pure . Password . Text.decodeUtf8 $ hashed

    getAccount :: DB.Connection -> Username -> IO (Maybe Account)
    getAccount conn (Username uname) = do
      listToMaybe <$> DB.queryNamed conn "SELECT * FROM accounts WHERE username = :username" [":username" := uname]

handleTokenList :: MonadIO m => ConnectionId -> [Text] -> BakamudServer m ()
handleTokenList cId@(ConnectionId connectionId) tokens = do
  connections <- asks _serverStateConnections
  maybeConnection  <- liftIO . atomically $ SMap.lookup cId connections

  case maybeConnection of
    Nothing -> error "Missing connection" -- TODO: log this
    Just connection -> do
      case _connectionState connection of
        Anonymous -> put cId "Please authenticate first."
        Authenticated -> do
          result <- withLuaInterpreterLock $ \lstate -> Lua.runWith @Lua.Exception lstate $ do
            _ <- Lua.getfield (-1) "handleCommand"
            _ <- Lua.pushinteger . Lua.Integer $ fromIntegral connectionId
            _ <- Lua.pushList Lua.pushText tokens
            r <- Lua.pcallTrace 2 0
            pure r
          when (result /= Just Lua.OK) $ do
            error "Lua code failed"
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
  maybeInterpreterLock <- lockLuaInterpreter
  case maybeInterpreterLock of
    Nothing -> pure Nothing
    Just interpreterState -> do
      returnValue <- liftIO $ callback interpreterState
      _ <- unlockLuaInterpreter interpreterState
      pure $ Just returnValue
