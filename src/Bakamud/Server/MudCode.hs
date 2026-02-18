{-# LANGUAGE RecordWildCards #-}

module Bakamud.Server.MudCode where

import Bakamud.Network.Connection
-- import qualified Bakamud.Server.Client as Client
import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as Q
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text.Encoding as Text
import HsLua.Core
import qualified StmContainers.Map as SMap
import System.FilePath

loadMain :: MonadIO m => BakamudServer m String
loadMain = do
  mainCodePath <- asks _serverStateMudMainPath
  liftIO . readFile $ mainCodePath </> "main.lua"

putConnection :: SMap.Map ConnectionId Connection -> HaskellFunction e
putConnection connections = do
  mRawConnectionId <- tointeger (nthBottom 1)
  mMsgBytes <- tostring (nthBottom 2)

  case (mRawConnectionId, mMsgBytes) of
    (Just rawConnectionId, Just msgBytes) -> do
      let connectionId = ConnectionId $ fromIntegral rawConnectionId
      liftIO . atomically $ do
        maybeConnection <- SMap.lookup connectionId connections
        case maybeConnection of
          Nothing -> undefined
          Just Connection {..} -> Q.writeTBQueue _connectionOutput $ Text.decodeUtf8 msgBytes
      pure 0
    _ -> Prelude.error "putConnection: invalid arguments"
