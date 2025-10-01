module Bakamud.Server where

import Bakamud.Server.State
import Control.Monad.Reader
import Data.Text (Text)

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
