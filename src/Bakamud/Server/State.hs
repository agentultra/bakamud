module Bakamud.Server.State where

import Bakamud.Network.Connection (ConnectionId, Connection)
import Colog.Core.Action
import Colog.Core.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Network.Socket

data ServerState m
  = ServerState
  { _serverStateHostName    :: Maybe HostName
  , _serverStatePort        :: ServiceName
  , _serverStateConnections :: Map ConnectionId Connection
  , _serverStateLogAction   :: LogAction m Text
  }

instance HasLog (ServerState m) Text m where
  getLogAction = _serverStateLogAction
  setLogAction action serverState =
    serverState { _serverStateLogAction = action }

emptyServerState
  :: Monad m
  => Maybe HostName
  -> ServiceName
  -> LogAction m Text
  -> ServerState m
emptyServerState mHostName serviceName logAction
  = ServerState
  { _serverStateHostName    = mHostName
  , _serverStatePort        = serviceName
  , _serverStateConnections = mempty
  , _serverStateLogAction   = logAction
  }
