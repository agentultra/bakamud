{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Server.MudCode where

import Bakamud.Account
import Bakamud.Avatar
import Bakamud.Network.Connection
import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as Q
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text.Encoding as Text
import Database.SQLite.Simple (NamedParam (..))
import qualified Database.SQLite.Simple as DB
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

  let connections = _serverStateConnections serverState
      dbHandle = _serverStateDbHandle serverState

  case mRawConnectionId of
    Just rawConnectionId -> do
      let connectionId = ConnectionId $ fromIntegral rawConnectionId
      maybeConnection <- liftIO . atomically $ do
        SMap.lookup connectionId connections
      case maybeConnection of
        Nothing -> Lua.pushstring "Invalid connectionId" *> Lua.error
        Just connection ->
          case _connectionAccountId connection of
            Nothing -> error "Unauthenticated user calling listAvatars" -- TODO: exception handling
            Just accountId ->
              lookupAvatars dbHandle accountId
    _ -> Lua.pushstring "Invalid connectionId" *> Lua.error
  where
    lookupAvatars :: Lua.LuaError e => TVar DB.Connection -> AccountId -> Lua.LuaE e Lua.NumResults
    lookupAvatars dbHandle accountId = do
      conn <- liftIO . atomically $ do
        readTVar dbHandle
      avatars <- liftIO $ DB.queryNamed conn "SELECT id, name, account_id FROM avatars WHERE account_id = :id" [":id" := accountId]
      Lua.pushList Lua.pushText (map _avatarName avatars)
      pure 1

exportFunctions :: Lua.LuaError e => [(ServerState -> HaskellFunction e, Name)]
exportFunctions =
  [ (putConnection, "put_connection")
  , (listAvatars, "list_avatars")
  ]
