module Main where

import Bakamud.Network.Server
import Bakamud.Server.State
import qualified Colog.Actions as Log

main :: IO ()
main = do
  let initialState = emptyServerState Log.logTextStdout
  runBakamudServer initialState (runTCPServer Nothing "3000")
