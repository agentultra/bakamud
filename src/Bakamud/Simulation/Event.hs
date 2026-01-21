module Bakamud.Simulation.Event where

import Bakamud.Network.Connection
import Data.Text (Text)

data SimEvent
  = ClientConnected ConnectionId
  | SomethingHappened Text
  deriving (Eq, Show)
