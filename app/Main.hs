module Main where

import Bakamud.Network.Server (runTCPServer, talk)

main :: IO ()
main = runTCPServer Nothing "3000" talk
