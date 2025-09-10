module Bakamud.Server.State where

import Bakamud.Network.Connection (ConnectionId, Connection)
import Data.Map.Strict (Map)

data ServerState
  = ServerState
  { _serverStateConnections :: Map ConnectionId Connection
  }
  deriving (Eq)

emptyServerState :: ServerState
emptyServerState = ServerState { _serverStateConnections = mempty }
