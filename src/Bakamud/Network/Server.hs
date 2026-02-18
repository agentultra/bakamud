{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bakamud.Network.Server where

import Bakamud.Auth
import Bakamud.Monad.Reader (bracketBakamudServer, bracketOnErrorBakamudServer, forkBakamud)
import Bakamud.Network.Connection (ConnectionId (..), Connection (..))
import Bakamud.Server
import Bakamud.Server.Monad
import Bakamud.Server.MudCode
import Bakamud.Server.State
import Bakamud.Server.Command
import Bakamud.Simulation
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as TB
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Exception as E
import Control.Monad (forever, when, void, forM_)
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Focus as Focus
import qualified HsLua.Core as Lua
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Text.Megaparsec as Parser
import qualified StmContainers.Map as SMap

runTCPServer :: Maybe HostName -> ServiceName -> BakamudServer IO a
runTCPServer mhost port = do
    addr <- liftIO resolve
    mudMainModule <- loadMain
    connMap <- asks _serverStateConnections
    loadCodeResult <- withLuaInterpreterLock $ \state -> Lua.runWith @Lua.Exception state $ do
      Lua.openlibs
      Lua.pushHaskellFunction (putConnection connMap)
      Lua.setglobal "put_connection"
      _ <- Lua.loadstring $ S8.pack mudMainModule
      Lua.pcall 0 1 Nothing

    when (loadCodeResult /= Just Lua.OK) $ do
      error "Could not load Lua code"
    _ <- forkBakamud simulation (const $ pure ())
    _ <- forkBakamud simulationOutbox (const $ pure ())
    _ <- forkBakamud commandDispatch (const $ pure ())
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
      bchan <- asks _serverStateBroadcastChannel
      -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
      -- but 'E.bracketOnError' above will be necessary if some
      -- non-atomic setups (e.g. spawning a subprocess to handle
      (clientConnectionId, clientConnection) <- initConnection conn bchan
      liftIO $ putStrLn "Received Connection!"
      liftIO . atomically $ TB.writeTBQueue (_connectionOutput clientConnection) "Welcome to BakaMUD!\n"
      _ <- forkBakamud (output clientConnection) (const $ gracefulClose conn 5000)
      forkBakamud (talk clientConnectionId clientConnection) (const $ gracefulClose conn 5000)

    initConnection :: Socket -> TChan Text -> BakamudServer IO (ConnectionId, Connection)
    initConnection s bchan = do
      inputQ <- liftIO $ newTBQueueIO 2
      outputQ <- liftIO $ newTBQueueIO 100
      broadcastChan <- liftIO . atomically $ dupTChan bchan
      connectionId <- nextConnectionId
      let connection = Connection Anonymous s inputQ outputQ broadcastChan
      addConnection connectionId connection
      pure (connectionId, connection)

talk :: ConnectionId -> Connection -> BakamudServer IO ()
talk connectionId c@Connection {..} = do
  msg <- liftIO $ recv _connectionSocket 1024
  commandQ <- asks _serverStateCommandQueue

  when (not . S.null $ msg) . liftIO . atomically $ do
    case Parser.parse commandP "" $ T.decodeUtf8 msg of
      Left parseError -> do
        let errMsg = T.pack $ Parser.errorBundlePretty parseError
        TQ.writeTQueue commandQ (connectionId, HandleParseError errMsg)
      Right command -> do
        TQ.writeTQueue commandQ (connectionId, command)
  talk connectionId c

output :: Connection -> BakamudServer IO ()
output c@Connection {..} = do
  outputQEmpty <- liftIO . atomically $ TB.isEmptyTBQueue _connectionOutput
  when (not outputQEmpty) $ do
    outputMsg <- liftIO . atomically $ readTBQueue _connectionOutput
    liftIO $ sendAll _connectionSocket $ T.encodeUtf8 outputMsg
  broadcastMsg <- liftIO . atomically $ tryReadTChan _connectionBroadcast
  when (isJust broadcastMsg) $ do
    let msg = head $ maybeToList broadcastMsg
    liftIO $ sendAll _connectionSocket $ T.encodeUtf8 msg
  output c

addConnection
  :: MonadIO m
  => ConnectionId
  -> Connection
  -> BakamudServer m ()
addConnection connectionId connection = do
  connectionsMap <- asks _serverStateConnections
  liftIO . atomically $ do
    let maybeAddFocus = Focus.alter maybeAdd
    SMap.focus maybeAddFocus connectionId connectionsMap
  where
    maybeAdd :: Maybe Connection -> Maybe Connection
    maybeAdd Nothing = Just connection
    maybeAdd (Just existingConnection) = Just existingConnection

commandDispatch :: BakamudServer IO ()
commandDispatch = do
  commandQ <- asks _serverStateCommandQueue
  commands <- liftIO . atomically $ TQ.flushTQueue commandQ
  forM_ commands $ \command -> do
    dispatchCommand command
  commandDispatch

simulationOutbox :: BakamudServer IO ()
simulationOutbox = do
  outChan <- asks _serverStateSimulationOutChan

  _ <- liftIO . atomically $ tryReadTChan outChan

  liftIO $ threadDelay 100000
  simulationOutbox
