{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Server.MudCode where

import Bakamud.Network.Connection
import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as Q
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text.Encoding as Text
import HsLua.Core (HaskellFunction, Name)
import qualified HsLua.Core as Lua
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

exportFunctions :: [(ServerState -> HaskellFunction e, Name)]
exportFunctions = [(putConnection, "put_connection")]
