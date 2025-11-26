module Bakamud.Simulation.Space where

import Data.Text (Text)
import Data.Map.Strict (Map)

data Exit
  = Exit
  { exitName :: Text
  , goesTo :: Text
  }
  deriving (Eq, Show)

data Room
  = Room
  { roomName :: Text
  , roomDescription :: Text
  , roomExits :: Map Text Exit
  }
  deriving (Eq, Show)
