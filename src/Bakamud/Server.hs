module Bakamud.Server where

import Bakamud.Network.Connection
import Bakamud.Server.State
import Control.Concurrent.STM
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

nextConnectionId :: MonadIO m => BakamudServer m ConnectionId
nextConnectionId = do
  connectionLimit <- asks _serverStateConnectionLimit
  connectionNextId <- asks _serverStateConnectionNextId
  liftIO . atomically . stateTVar connectionNextId $ \currentId -> do
    let nextId = if currentId == connectionLimit then 0 else currentId + 1
    (ConnectionId $ fromIntegral nextId, nextId)
