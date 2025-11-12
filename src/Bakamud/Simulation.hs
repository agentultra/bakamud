{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bakamud.Simulation where

import Bakamud.Server.Monad
import Bakamud.Server.MudCode
import Bakamud.Server.State
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Int
import Data.Time.Clock.System
import qualified Lua as Lua
import Foreign.C.String
import Foreign.C.Types

simulation :: MonadIO m => BakamudServer m ()
simulation = do
  mudMainModule <- loadMain
  lTVar <- asks _serverStateLuaInterpreterState
  l <- liftIO . atomically $ readTVar lTVar
  liftIO $ Lua.luaL_openlibs l
  result <- liftIO $ withCString "main" $ \name -> do
    (codePtr, codeLen) <- newCStringLen mudMainModule
    Lua.luaL_loadbuffer l codePtr (CSize $ fromIntegral codeLen) name
  if result /= Lua.LUA_OK
    then error "Error loading MUD code, aborting..."
    else tick l

tick :: MonadIO m => Lua.State -> BakamudServer m ()
tick l = do
  startTime <- asks _serverStateSimStartTime
  timeRate <- asks _serverStateSimTimeRate
  currTime <- systemSeconds <$> liftIO getSystemTime
  accTimeTVar <- asks _serverStateSimDeltaTimeAccumMs
  accTime <- liftIO . atomically . readTVar $ accTimeTVar
  let dt = currTime - startTime
  when (accTime + dt > timeRate) $ do
    _ <- liftIO $ Lua.lua_pcall l (Lua.NumArgs 0) (Lua.NumResults 0) (Lua.StackIndex 0)
    pure ()
  accumulateDt dt accTimeTVar
  tick l
  where
    accumulateDt :: MonadIO m => Int64 -> TVar Int64 -> BakamudServer m ()
    accumulateDt dt accTimeTVar = liftIO . atomically $ do
      modifyTVar' accTimeTVar $ \accTime -> accTime + dt
