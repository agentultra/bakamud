{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bakamud.Network.Server where

import Bakamud.Auth
import Bakamud.Network.Connection (ConnectionId (..), Connection (..))
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

data ServerState
  = ServerState
  { _serverStateConnections :: Map ConnectionId Connection
  }
  deriving (Eq, Show)

emptyServerState :: ServerState
emptyServerState = ServerState { _serverStateConnections = mempty }

runTCPServer :: Maybe HostName -> ServiceName -> (TVar ServerState -> Socket -> IO a) -> IO a
runTCPServer mhost port server = do
    addr <- resolve
    serverState <- newTVarIO emptyServerState
    E.bracket (open addr) close $ loop serverState
  where
    resolve :: IO AddrInfo
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        NE.head <$> getAddrInfo (Just hints) mhost (Just port)

    open :: AddrInfo -> IO Socket
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock

    loop :: TVar ServerState -> Socket -> IO a
    loop serverState sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server serverState conn) (const $ gracefulClose conn 5000)

talk :: TVar ServerState -> Socket -> IO ()
talk serverState s = do
  sendAll s "Welcome to Bakamud!\n"
  msg <- recv s 1024
  unless (S.null msg) $ do
    case msg of
      "auth\r\n" -> do
        let cid = ConnectionId 1
            connection = Connection Anonymous
        atomically $ modifyTVar serverState (addConnection cid connection)
      "yo\r\n" -> do
        ss <- atomically $ readTVar serverState
        sendAll s . C.pack $ show ss
      _ -> sendAll s msg
    talk serverState s

addConnection
  :: ConnectionId
  -> Connection
  -> ServerState
  -> ServerState
addConnection cId c s =
  s { _serverStateConnections = Map.alter maybeAdd cId s._serverStateConnections }
  where
    maybeAdd :: Maybe Connection -> Maybe Connection
    maybeAdd Nothing = Just c
    maybeAdd (Just existingC) = Just existingC
