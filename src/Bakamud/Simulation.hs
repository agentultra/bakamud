{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bakamud.Simulation where

import Bakamud.Server.Monad
import Bakamud.Server.MudCode
import Bakamud.Server.State
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Lua
import Foreign.C.String
import Foreign.C.Types

simulation :: MonadIO m => BakamudServer m ()
simulation = do
  mudMainModule <- loadMain
  lTVar <- asks _serverStateLuaInterpreterState
  l <- liftIO . atomically $ readTVar lTVar
  liftIO $ luaL_openlibs l
  liftIO $ withCString "main" $ \name -> do
    (codePtr, codeLen) <- newCStringLen mudMainModule
    result <- luaL_loadbuffer l codePtr (CSize $ fromIntegral codeLen) name
    if result /= LUA_OK
      then error "Error loading MUD code, aborting..."
      else tick l

tick :: State -> IO ()
tick l = do
  _ <- lua_pcall l (NumArgs 0) (NumResults 0) (StackIndex 0)
  threadDelay 3000000
  tick l
