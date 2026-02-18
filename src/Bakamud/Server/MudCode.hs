module Bakamud.Server.MudCode where

-- import Bakamud.Network.Connection (ConnectionId (..))
-- import qualified Bakamud.Server.Client as Client
import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Text.Encoding as Text
import HsLua.Core
import System.FilePath

import qualified Debug.Trace as Debug

loadMain :: MonadIO m => BakamudServer m String
loadMain = do
  mainCodePath <- asks _serverStateMudMainPath
  liftIO . readFile $ mainCodePath </> "main.lua"

putConnection :: HaskellFunction e
putConnection = do
  mRawConnectionId <- tointeger (nthBottom 1)
  mMsgBytes <- tostring (nthBottom 2)

  Debug.traceM $ "putConnection: " ++ show mRawConnectionId ++ show (BS.unpack <$> mMsgBytes)
  case (mRawConnectionId, mMsgBytes) of
    (Just rawConnectionId, Just msgBytes) -> do
      -- Client.put (ConnectionId rawConnectionId) Text.decodeUtf8
      Debug.traceM $ "putConnection - Connection Id: " ++ show rawConnectionId ++ " msg: " ++ BS.unpack msgBytes
      pure 0
    _ -> Prelude.error "putConnection: invalid arguments"
