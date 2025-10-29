{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bakamud.Simulation where

import Bakamud.Server.Monad
import Bakamud.Server.MudCode
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Lua
import Foreign.C.String
import Foreign.C.Types

simulation :: MonadIO m => BakamudServer m ()
simulation = do
  mudMainModule <- loadMain
  liftIO $ print mudMainModule
  liftIO $ withNewState $ \l -> do
    luaL_openlibs l
    withCString "main" $ \name -> do
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
