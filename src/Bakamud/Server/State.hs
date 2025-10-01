module Bakamud.Server.State where

import Bakamud.Network.Connection (ConnectionId, Connection)
import Control.Concurrent.STM.TVar
import Data.Map.Strict (Map)
import Network.Socket

data ServerState m
  = ServerState
  { _serverStateHostName    :: Maybe HostName
  , _serverStatePort        :: ServiceName
  , _serverStateConnections :: TVar (Map ConnectionId Connection)
  }

emptyServerState
  :: Monad m
  => Maybe HostName
  -> ServiceName
  -> IO (ServerState m)
emptyServerState mHostName serviceName = do
  serverStateTVar <- newTVarIO $ mempty
  pure
    $ ServerState
    { _serverStateHostName    = mHostName
    , _serverStatePort        = serviceName
    , _serverStateConnections = serverStateTVar
    }
