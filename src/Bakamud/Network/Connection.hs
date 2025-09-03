module Bakamud.Network.Connection where

import Bakamud.Auth

data Connection
  = Connection
  { _connectionState :: AuthState
  }
  deriving (Eq, Show)
