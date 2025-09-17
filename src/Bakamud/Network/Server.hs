{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Network.Server where

import Bakamud.Auth
import Bakamud.Network.Connection (ConnectionId (..), Connection (..))
import Bakamud.Server.State
import Bakamud.Simulation
import Colog.Core.Action (LogAction (..))
import qualified Colog.Core.Class as Log
import qualified Colog.Actions as Log
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as TB
import qualified Control.Exception as E
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

runTCPServer :: Maybe HostName -> ServiceName -> IO a
runTCPServer mhost port = do
    addr <- liftIO resolve
    serverState <- liftIO . newTVarIO $ emptyServerState Log.logTextStdout
    _ <- liftIO $ forkFinally (simulation serverState) (const $ pure ())
    liftIO $ E.bracket (liftIO $ open addr) (liftIO . close) $ loop serverState
  where
    resolve :: IO AddrInfo
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        NE.head <$> getAddrInfo (Just hints) mhost (Just port)

    open :: AddrInfo -> IO Socket
    open addr = liftIO $ E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock

    loop :: TVar (ServerState IO) -> Socket -> IO a
    loop serverState sock = do
      forever $ liftIO $ E.bracketOnError (liftIO $ accept sock) (liftIO . close . fst)
        $ connect_ serverState

    connect_ :: TVar (ServerState IO) -> (Socket, SockAddr) -> IO ()
    connect_ serverState (conn, _peer) = void $ do
      -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
      -- but 'E.bracketOnError' above will be necessary if some
      -- non-atomic setups (e.g. spawning a subprocess to handle
      clientConnection <- initConnection serverState conn
      ss <- readTVarIO serverState
      let LogAction l = Log.getLogAction @_ @Text ss
      l "Received Connection!"
      atomically $ TB.writeTBQueue (_connectionOutput clientConnection) "Welcome to BakaMUD!\n"
      _ <- forkFinally (output clientConnection) (const $ gracefulClose conn 5000)
      forkFinally (talk clientConnection) (const $ gracefulClose conn 5000)

    initConnection :: TVar (ServerState IO) -> Socket -> IO Connection
    initConnection serverState s = do
      inputQ <- newTBQueueIO 2
      outputQ <- newTBQueueIO 100
      let cid = ConnectionId 1
          connection = Connection Anonymous s inputQ outputQ
      atomically $ modifyTVar serverState (addConnection cid connection)
      pure connection

talk :: Connection -> IO ()
talk c@Connection {..} = do
  msg <- recv _connectionSocket 1024
  when (not . S.null $ msg) . atomically $ do
    inputQFull <- TB.isFullTBQueue _connectionInput
    if inputQFull
      then TB.writeTBQueue _connectionOutput "Command input queue is full please wait and try again."
      else TB.writeTBQueue _connectionInput $ T.decodeUtf8 msg
  talk c

output :: Connection -> IO ()
output c@Connection {..} = do
  outputQEmpty <- atomically $ TB.isEmptyTBQueue _connectionOutput
  when (not outputQEmpty) $ do
    outputMsg <- atomically $ readTBQueue _connectionOutput
    sendAll _connectionSocket $ T.encodeUtf8 outputMsg
  output c

addConnection
  :: Monad m
  => ConnectionId
  -> Connection
  -> ServerState m
  -> ServerState m
addConnection cId c s =
  s { _serverStateConnections = Map.alter maybeAdd cId s._serverStateConnections }
  where
    maybeAdd :: Maybe Connection -> Maybe Connection
    maybeAdd Nothing = Just c
    maybeAdd (Just existingC) = Just existingC
