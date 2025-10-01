module Main where

import Bakamud.Network.Server
import Bakamud.Server
import Bakamud.Server.State

main :: IO ()
main = do
  initialState <- emptyServerState Nothing "3000"
  runBakamudServer initialState (runTCPServer Nothing "3000")
