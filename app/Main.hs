module Main where

import Bakamud.Network.Server (runTCPServer)

main :: IO ()
main = runTCPServer Nothing "3000"
