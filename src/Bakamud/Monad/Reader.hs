module Bakamud.Monad.Reader where

import Bakamud.Server
import Control.Monad.Reader
import Control.Exception

bracketBakamudServer :: BakamudServer IO a -> (a -> BakamudServer IO b) -> (a -> BakamudServer IO c) -> BakamudServer IO c
bracketBakamudServer a cb cc = do
    r <- ask
    liftIO $ bracket (runBakamudServer r a) (\x -> runBakamudServer r (cb x)) (\x -> runBakamudServer r (cc x))
