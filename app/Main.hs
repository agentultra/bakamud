module Main where

import Bakamud.Network.Server
import Bakamud.Server.Monad
import Bakamud.Server.State

main :: IO ()
main = do
  initialState <- initServerState Nothing "3000" "/opt/bakamud"
  runBakamudServer initialState (runTCPServer Nothing "3000")
