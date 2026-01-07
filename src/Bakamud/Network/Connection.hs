module Bakamud.Network.Connection where

import Bakamud.Auth
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TChan (TChan)
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Network.Socket (Socket)

newtype ConnectionId = ConnectionId { getConnectionId :: Integer }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ConnectionId

data Connection
  = Connection
  { _connectionState :: AuthState
  , _connectionSocket :: Socket
  , _connectionInput :: TBQueue Text
  , _connectionOutput :: TBQueue Text
  , _connectionBroadcast :: TChan Text
  }
  deriving (Eq)
