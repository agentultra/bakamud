module Main where

import Control.Monad (unless)
import qualified Data.ByteString as S
import Bakamud.Network.Server (runTCPServer)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          sendAll s msg
          talk s
