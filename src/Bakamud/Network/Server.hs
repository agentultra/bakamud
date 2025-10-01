{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Network.Server where

import Bakamud.Auth
import Bakamud.Monad.Reader (bracketBakamudServer, bracketOnErrorBakamudServer, forkBakamud)
import Bakamud.Network.Connection (ConnectionId (..), Connection (..))
import Bakamud.Server
import Bakamud.Server.State
import Bakamud.Simulation
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as TB
import qualified Control.Exception as E
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString as S
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

runTCPServer :: Maybe HostName -> ServiceName -> BakamudServer IO a
runTCPServer mhost port = do
    addr <- liftIO resolve
    _ <- forkBakamud (simulation) (const $ pure ())
    bracketBakamudServer (open addr) (close_) loop
  where
    resolve :: IO AddrInfo
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        NE.head <$> getAddrInfo (Just hints) mhost (Just port)

    open :: AddrInfo -> BakamudServer IO Socket
    open addr = liftIO $ E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock

    close_ :: Socket -> BakamudServer IO ()
    close_ = liftIO . close

    loop :: Socket -> BakamudServer IO a
    loop sock = do
      forever $ bracketOnErrorBakamudServer (liftIO $ accept sock) (liftIO . close . fst)
        $ connect_

    connect_ :: (Socket, SockAddr) -> BakamudServer IO ()
    connect_ (conn, _peer) = void $ do
      -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
      -- but 'E.bracketOnError' above will be necessary if some
      -- non-atomic setups (e.g. spawning a subprocess to handle
      clientConnection <- initConnection conn
      --let LogAction l = Log.getLogAction @_ @Text serverState
      liftIO $ putStrLn "Received Connection!"
      liftIO . atomically $ TB.writeTBQueue (_connectionOutput clientConnection) "Welcome to BakaMUD!\n"
      _ <- forkBakamud (output clientConnection) (const $ gracefulClose conn 5000)
      forkBakamud (talk clientConnection) (const $ gracefulClose conn 5000)

    initConnection :: Socket -> BakamudServer IO Connection
    initConnection s = do
      inputQ <- liftIO $ newTBQueueIO 2
      outputQ <- liftIO $ newTBQueueIO 100
      connectionId <- nextConnectionId
      let connection = Connection Anonymous s inputQ outputQ
      addConnection connectionId connection
      -- atomically $ modifyTVar serverState (addConnection cid connection)
      pure connection

talk :: Connection -> BakamudServer IO ()
talk c@Connection {..} = do
  msg <- liftIO $ recv _connectionSocket 1024
  when (not . S.null $ msg) . liftIO . atomically $ do
    inputQFull <- TB.isFullTBQueue _connectionInput
    if inputQFull
      then TB.writeTBQueue _connectionOutput "Command input queue is full please wait and try again."
      else TB.writeTBQueue _connectionInput $ T.decodeUtf8 msg
  talk c

output :: Connection -> BakamudServer IO ()
output c@Connection {..} = do
  outputQEmpty <- liftIO . atomically $ TB.isEmptyTBQueue _connectionOutput
  when (not outputQEmpty) $ do
    outputMsg <- liftIO . atomically $ readTBQueue _connectionOutput
    liftIO $ sendAll _connectionSocket $ T.encodeUtf8 outputMsg
  output c

addConnection
  :: MonadIO m
  => ConnectionId
  -> Connection
  -> BakamudServer m ()
addConnection connectionId connection = do
  connectionsTVar <- asks _serverStateConnections
  liftIO . atomically . modifyTVar' connectionsTVar $ \connections -> do
    Map.alter maybeAdd connectionId connections
  where
    maybeAdd :: Maybe Connection -> Maybe Connection
    maybeAdd Nothing = Just connection
    maybeAdd (Just existingConnection) = Just existingConnection
