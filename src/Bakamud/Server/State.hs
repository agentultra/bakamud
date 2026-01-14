{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bakamud.Server.State where

import Bakamud.Network.Connection (ConnectionId, Connection)
import Bakamud.Server.Command
import Bakamud.Simulation.Event
import Bakamud.Simulation.Space
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad
--import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Lua (State)
import qualified Lua.Ersatz.Auxiliary as Lua
import Network.Socket
import qualified StmContainers.Map as SMap
import System.FilePath
import System.Directory

data ServerState m
  = ServerState
  { _serverStateHostName            :: Maybe HostName
  , _serverStatePort                :: ServiceName
  , _serverStateConnectionLimit     :: Word
  , _serverStateConnectionNextId    :: TVar Word
  , _serverStateConnections         :: SMap.Map ConnectionId Connection
  , _serverStateBroadcastChannel    :: TChan Text
  , _serverStateCommandQueue        :: TQueue (ConnectionId, Command)
  , _serverStateAccounts            :: TVar (Map Username Password)
  , _serverStateMudMainPath         :: FilePath
  , _serverStateLuaInterpreterState :: TVar State
  -- ^ Path to main user MUD-code module to run simulation
  , _serverStateSimRooms            :: Map Text Room
  , _serverStateSimulationOutChan   :: TChan SimEvent
  }

initServerState
  :: Monad m
  => Maybe HostName
  -> ServiceName
  -> FilePath
  -> IO (ServerState m)
initServerState mHostName serviceName mudMainPath = do
  mudMainPathExists <- doesDirectoryExist mudMainPath
  when (not $ mudMainPathExists) $
    error $ "Invalid mud code directory: " ++ mudMainPath
  nextIdTVar <- newTVarIO 0
  serverStateConnectionsMap <- SMap.newIO
  serverStateAccounts <- newTVarIO $ mempty
  bchan <- newBroadcastTChanIO
  commandQ <- newTQueueIO
  luaState <- Lua.hsluaL_newstate
  luaStateTVar <- newTVarIO luaState
  roomMap <- compileRooms mudMainPath
  simOutChan <- newTChanIO
  pure
    $ ServerState
    { _serverStateHostName            = mHostName
    , _serverStatePort                = serviceName
    , _serverStateConnectionLimit     = 65535
    , _serverStateConnectionNextId    = nextIdTVar
    , _serverStateConnections         = serverStateConnectionsMap
    , _serverStateBroadcastChannel    = bchan
    , _serverStateCommandQueue        = commandQ
    , _serverStateAccounts            = serverStateAccounts
    , _serverStateMudMainPath         = mudMainPath
    , _serverStateLuaInterpreterState = luaStateTVar
    , _serverStateSimRooms            = roomMap
    , _serverStateSimulationOutChan   = simOutChan
    }

-- | Read the mudMainPath directory for Room, Exit definitions and
-- load them into the state.
compileRooms :: FilePath -> IO (Map Text Room)
compileRooms basePath = do
  roomFiles <- listDirectory $ basePath </> "rooms"
  Map.fromList <$> traverse loadRoom roomFiles
  where
    loadRoom :: FilePath -> IO (Text, Room)
    loadRoom fname = do
      source <- Text.readFile $ basePath </> "rooms" </> fname
      case parseRoomDefinition source of
        Left err -> error $ "Error loading room definition (" ++ show err ++ "): " ++ fname
        Right def -> pure def
