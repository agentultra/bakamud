module Bakamud.Monad.Reader where

import Bakamud.Server
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader

bracketBakamudServer :: BakamudServer IO a -> (a -> BakamudServer IO b) -> (a -> BakamudServer IO c) -> BakamudServer IO c
bracketBakamudServer a cb cc = do
    r <- ask
    liftIO $ bracket (runBakamudServer r a) (\x -> runBakamudServer r (cb x)) (\x -> runBakamudServer r (cc x))

bracketOnErrorBakamudServer :: BakamudServer IO a -> (a -> BakamudServer IO b) -> (a -> BakamudServer IO c) -> BakamudServer IO c
bracketOnErrorBakamudServer a cb cc = do
    r <- ask
    liftIO $ bracketOnError (runBakamudServer r a) (\x -> runBakamudServer r (cb x)) (\x -> runBakamudServer r (cc x))

forkBakamud :: BakamudServer IO a -> (Either SomeException a -> IO ()) -> BakamudServer IO ThreadId
forkBakamud server handler = do
  r <- ask
  liftIO $ forkFinally (runBakamudServer r server) handler
