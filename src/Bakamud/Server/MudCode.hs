module Bakamud.Server.MudCode where

import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Monad.IO.Class
import Control.Monad.Reader

loadMain :: MonadIO m => BakamudServer m String
loadMain = do
  mainCodePath <- asks _serverStateMudMainPath
  liftIO $ readFile mainCodePath
