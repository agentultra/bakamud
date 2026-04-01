{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bakamud.Server.MudCode where

import Bakamud.Account
import Bakamud.Avatar
import Bakamud.Network.Connection
import Bakamud.Server.Monad
import Bakamud.Server.State
import Bakamud.Session
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as Q
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe (listToMaybe)
import qualified Data.Text.Encoding as Text
import Database.SQLite.Simple (NamedParam (..))
import qualified Database.SQLite.Simple as DB
import qualified Focus as Focus
import HsLua.Core (HaskellFunction, Name)
import qualified HsLua.Core as Lua
import qualified HsLua.Marshalling as Lua
import qualified StmContainers.Map as SMap
import System.FilePath

loadMain :: MonadIO m => BakamudServer m String
loadMain = do
  mainCodePath <- asks _serverStateMudMainPath
  liftIO . readFile $ mainCodePath </> "main.lua"

putConnection :: ServerState -> HaskellFunction e
putConnection serverState = do
  mRawConnectionId <- Lua.tointeger (Lua.nthBottom 1)
  mMsgBytes <- Lua.tostring (Lua.nthBottom 2)
  let connections = _serverStateConnections serverState

  case (mRawConnectionId, mMsgBytes) of
    (Just rawConnectionId, Just msgBytes) -> do
      let connectionId = ConnectionId $ fromIntegral rawConnectionId
      ok <- liftIO . atomically $ do
        maybeConnection <- SMap.lookup connectionId connections
        case maybeConnection of
          Nothing -> pure False
          Just Connection {..} -> do
            Q.writeTBQueue _connectionOutput $ Text.decodeUtf8 msgBytes
            pure True
      if ok
        then pure 0
        else Lua.pushstring "Connection no longer available" *> Lua.error
    _ -> Prelude.error "putConnection: invalid arguments"

listAvatars :: Lua.LuaError e => ServerState -> HaskellFunction e
listAvatars serverState = do
  mRawConnectionId <- Lua.tointeger (Lua.nthBottom 1)

  let sessions = _serverStateSessions serverState
      dbHandle = _serverStateDbHandle serverState

  case mRawConnectionId of
    Just rawConnectionId -> do
      let connectionId = ConnectionId $ fromIntegral rawConnectionId
      maybeSession <- liftIO . atomically $ do
        SMap.lookup connectionId sessions
      case maybeSession of
        Nothing -> Lua.pushstring "Invalid connectionId" *> Lua.error
        Just session ->
          case _sessionAccountId session of
            Nothing -> error "Unauthenticated user calling listAvatars" -- TODO: exception handling
            Just accountId ->
              lookupAvatars dbHandle accountId
    _ -> Lua.pushstring "Invalid connectionId" *> Lua.error
  where
    lookupAvatars :: Lua.LuaError e => TVar DB.Connection -> AccountId -> Lua.LuaE e Lua.NumResults
    lookupAvatars dbHandle accountId = do
      conn <- liftIO . atomically $ do
        readTVar dbHandle
      avatars <- liftIO $ DB.queryNamed @Avatar conn "SELECT id, name, account_id FROM avatars WHERE account_id = :id" [":id" := accountId]
      Lua.pushList (Lua.pushAsTable [("name", pushAvatarName), ("id", pushAvatarId)]) avatars
      pure 1

    pushAvatarName :: Lua.LuaError e => Avatar -> Lua.LuaE e ()
    pushAvatarName Avatar {..} = Lua.pushText _avatarName

    pushAvatarId :: Lua.LuaError e => Avatar -> Lua.LuaE e ()
    pushAvatarId Avatar {..} =
      let (AvatarId avatarId) = _avatarId
      in Lua.pushIntegral avatarId

setAvatar :: Lua.LuaError e => ServerState -> HaskellFunction e
setAvatar serverState = do
  rawConnectionId <- getArgument Lua.tointeger (Lua.nthBottom 1)
  (Lua.Integer rawAvatarId) <- getArgument Lua.tointeger (Lua.nthBottom 2)

  let connectionId = ConnectionId $ fromIntegral rawConnectionId
  session <- getValidSession serverState connectionId
  accountId <- requireAccountId session

  conn <- liftIO . atomically $ do
    readTVar $ _serverStateDbHandle serverState
  avatars <- liftIO $ DB.queryNamed @Avatar conn "SELECT id, name, account_id FROM avatars WHERE account_id = :account_id AND id = :id" [":account_id" := accountId, ":id" := rawAvatarId ]
  case listToMaybe avatars of
    Nothing -> Lua.pushstring "Invalid avatar" *> Lua.error
    Just avatar -> do
      liftIO . atomically $ do
        let updateAuthSuccessFocus
              = Focus.adjust (updateSession avatar)
        SMap.focus updateAuthSuccessFocus connectionId
          $ _serverStateSessions serverState
      pure 0
  where
    updateSession :: Avatar -> Session -> Session
    updateSession avatar session =
      session { _sessionAvatarId = Just $ _avatarId avatar }

-- Helpers

getArgument
  :: Lua.LuaError e
  => (Lua.StackIndex -> Lua.LuaE e (Maybe a))
  -> Lua.StackIndex
  -> Lua.LuaE e a
getArgument fromLuaStack stackIx = do
  maybeArgument <- fromLuaStack stackIx
  case maybeArgument of
    Nothing -> Lua.failLua "Missing argument"
    Just argument -> pure argument

getValidConnection
  :: Lua.LuaError e
  => ServerState
  -> ConnectionId
  -> Lua.LuaE e Connection
getValidConnection serverState connectionId = do
  let connections = _serverStateConnections serverState
  maybeConnection <- liftIO . atomically $ do
    SMap.lookup connectionId connections
  case maybeConnection of
    Nothing -> Lua.failLua "Invalid connection"
    Just connection -> pure connection

getValidSession
  :: Lua.LuaError e
  => ServerState
  -> ConnectionId
  -> Lua.LuaE e Session
getValidSession serverState connectionId = do
  let sessions = _serverStateSessions serverState
  maybeConnection <- liftIO . atomically $ do
    SMap.lookup connectionId sessions
  case maybeConnection of
    Nothing -> Lua.failLua "Invalid connection"
    Just session -> pure session

requireAccountId :: Lua.LuaError e => Session -> Lua.LuaE e AccountId
requireAccountId session = do
  case _sessionAccountId session of
    Nothing -> error "assert requireAccountId: should never happen"
    Just accountId -> pure accountId

exportFunctions :: Lua.LuaError e => [(ServerState -> HaskellFunction e, Name)]
exportFunctions =
  [ (putConnection, "put_connection")
  , (listAvatars, "list_avatars")
  , (setAvatar, "set_avatar")
  ]
