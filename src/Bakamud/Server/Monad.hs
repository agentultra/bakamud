module Bakamud.Server.Monad where

import Bakamud.Server.State
import Control.Monad.Reader

newtype BakamudServer m a
  = BakamudServer
  { getBakamudServer :: ReaderT (ServerState m) m a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader (ServerState m)
    )

runBakamudServer :: MonadIO m => ServerState m -> BakamudServer m a -> m a
runBakamudServer serverState = (`runReaderT` serverState) . getBakamudServer
