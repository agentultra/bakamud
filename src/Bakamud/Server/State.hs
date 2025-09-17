module Bakamud.Server.State where

import Bakamud.Network.Connection (ConnectionId, Connection)
import Colog.Core.Action
import Colog.Core.Class
import Data.Map.Strict (Map)
import Data.Text (Text)

data ServerState m
  = ServerState
  { _serverStateConnections :: Map ConnectionId Connection
  , _serverStateLogAction :: LogAction m Text
  }

instance HasLog (ServerState m) Text m where
  getLogAction = _serverStateLogAction
  setLogAction action serverState =
    serverState { _serverStateLogAction = action }

emptyServerState :: Monad m => LogAction m Text -> ServerState m
emptyServerState logAction
  = ServerState
  { _serverStateConnections = mempty
  , _serverStateLogAction = logAction
  }
