module Bakamud.Server.Monad where

import Bakamud.Server.State
import Control.Monad.Reader

newtype BakamudServer m a
  = BakamudServer
  { getBakamudServer :: ReaderT ServerState m a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader ServerState
    )

runBakamudServer :: MonadIO m => ServerState -> BakamudServer m a -> m a
runBakamudServer serverState = (`runReaderT` serverState) . getBakamudServer
