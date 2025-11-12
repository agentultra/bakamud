module Bakamud.Server.State where

import Bakamud.Network.Connection (ConnectionId, Connection)
import Bakamud.Server.Command
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Data.Int
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock.System
import Network.Socket

data ServerState m
  = ServerState
  { _serverStateHostName            :: Maybe HostName
  , _serverStatePort                :: ServiceName
  , _serverStateConnectionLimit     :: Word
  , _serverStateConnectionNextId    :: TVar Word
  , _serverStateConnections         :: TVar (Map ConnectionId Connection)
  , _serverStateBroadcastChannel    :: TChan Text
  , _serverStateCommandQueue        :: TQueue (ConnectionId, Command)
  , _serverStateAccounts            :: TVar (Map Username Password)
  , _serverStateMudMainPath         :: FilePath
  , _serverStateSimStartTime        :: Int64
  , _serverStateSimTimeRate         :: Int -- ^ How many ms to accumulate before tick, ie: 100ms = 10 ticks/s
  , _serverStateSimDeltaTimeAccumMs :: Int
  -- ^ Path to main user MUD-code module to run simulation
  }

emptyServerState
  :: Monad m
  => Maybe HostName
  -> ServiceName
  -> FilePath
  -> IO (ServerState m)
emptyServerState mHostName serviceName mudMainPath = do
  nextIdTVar <- newTVarIO 0
  serverStateTVar <- newTVarIO $ mempty
  serverStateAccounts <- newTVarIO $ mempty
  bchan <- newBroadcastTChanIO
  commandQ <- newTQueueIO
  startTime <- getSystemTime
  pure
    $ ServerState
    { _serverStateHostName            = mHostName
    , _serverStatePort                = serviceName
    , _serverStateConnectionLimit     = 65535
    , _serverStateConnectionNextId    = nextIdTVar
    , _serverStateConnections         = serverStateTVar
    , _serverStateBroadcastChannel    = bchan
    , _serverStateCommandQueue        = commandQ
    , _serverStateAccounts            = serverStateAccounts
    , _serverStateMudMainPath         = mudMainPath
    , _serverStateSimStartTime        = systemSeconds startTime
    , _serverStateSimTimeRate         = 100 -- TODO: set this
    , _serverStateSimDeltaTimeAccumMs = 0
    }
