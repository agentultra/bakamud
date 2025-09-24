module Bakamud.Server where

import Control.Monad.Reader
import Bakamud.Server.State

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
