module Bakamud.Server.MudCode where

import Bakamud.Server.Monad
import Bakamud.Server.State
import Control.Monad.IO.Class
import Control.Monad.Reader
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Lua
import System.FilePath

loadMain :: MonadIO m => BakamudServer m String
loadMain = do
  mainCodePath <- asks _serverStateMudMainPath
  liftIO . readFile $ mainCodePath </> "main.lua"

echo :: PreCFunction
echo state = do
  s <- lua_tolstring state (nthTop 1) nullPtr
  fromLua <- peekCString s
  withCStringLen ("Hello from Haskell: " ++ fromLua) $ \(cStr, cStrLen) ->
    lua_pushlstring state cStr (CSize $ fromIntegral cStrLen)
  return $ NumResults 1
