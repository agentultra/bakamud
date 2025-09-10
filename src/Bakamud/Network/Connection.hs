module Bakamud.Network.Connection where

import Bakamud.Auth
import Control.Concurrent.STM.TBQueue (TBQueue)
import Data.Text (Text)
import Network.Socket (Socket)

newtype ConnectionId = ConnectionId { getConnectionId :: Integer }
  deriving (Eq, Ord, Show)

data Connection
  = Connection
  { _connectionState :: AuthState
  , _connectionSocket :: Socket
  , _connectionInput :: TBQueue Text
  , _connectionOutput :: TBQueue Text
  }
  deriving (Eq)
