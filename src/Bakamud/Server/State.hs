module Bakamud.Server.State where

import Bakamud.Network.Connection (ConnectionId, Connection)
import Control.Concurrent.STM.TVar
import Data.Map.Strict (Map)
import Network.Socket

data ServerState m
  = ServerState
  { _serverStateHostName         :: Maybe HostName
  , _serverStatePort             :: ServiceName
  , _serverStateConnectionLimit  :: Word
  , _serverStateConnectionNextId :: TVar Word
  , _serverStateConnections      :: TVar (Map ConnectionId Connection)
  }

emptyServerState
  :: Monad m
  => Maybe HostName
  -> ServiceName
  -> IO (ServerState m)
emptyServerState mHostName serviceName = do
  nextIdTVar <- newTVarIO 0
  serverStateTVar <- newTVarIO $ mempty
  pure
    $ ServerState
    { _serverStateHostName         = mHostName
    , _serverStatePort             = serviceName
    , _serverStateConnectionLimit  = 65535
    , _serverStateConnectionNextId = nextIdTVar
    , _serverStateConnections      = serverStateTVar
    }
