module Bakamud.Network.Connection where

import Bakamud.Auth

newtype ConnectionId = ConnectionId { getConnectionId :: Integer }
  deriving (Eq, Ord, Show)

data Connection
  = Connection
  { _connectionState :: AuthState
  }
  deriving (Eq, Show)
