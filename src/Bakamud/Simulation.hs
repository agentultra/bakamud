{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bakamud.Simulation where

import Bakamud.Network.Connection
import Bakamud.Server.Monad
import Bakamud.Server.MudCode
import Bakamud.Server.State
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Int
import Data.Time.Clock.System
import Data.Text (Text)
import qualified Lua as Lua
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr

data SimEvent
  = ClientConnected ConnectionId
  | SomethingHappened Text
  deriving (Eq, Show)

data SimulationState
  = SimulationState
  { _simulationStateOutChan          :: TChan SimEvent
  , _simulationStateLuaInterpreter   :: TVar Lua.State
  , _simulationStateLastTime         :: TVar Int64
  , _simulationStateTimeRate         :: Int64 -- ^ How many ms to accumulate before tick, ie: 100ms = 10 ticks/s
  , _simulationStateDeltaTimeAccumMs :: TVar Int64
  }
  deriving (Eq)

newtype Simulation m a = Simulation { getSimulation :: StateT SimulationState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState SimulationState)

runSimulation :: Monad m => SimulationState -> Simulation m a -> m a
runSimulation initialState = (`evalStateT` initialState) . getSimulation

simulation :: MonadIO m => BakamudServer m ()
simulation = do
  mudMainModule <- loadMain
  lTVar <- asks _serverStateLuaInterpreterState
  -- TODO: Remove this, initialize interpreter state higher up. This
  -- is left-over prototype code.
  l <- liftIO . atomically $ readTVar lTVar
  outChan <- liftIO $ newTChanIO
  liftIO $ do
    Lua.hslua_pushhsfunction l echo
    withCStringLen "echo" $ \(funName, funNameLen) ->
      Lua.hslua_setglobal l funName (CSize $ fromIntegral funNameLen) nullPtr
    Lua.luaL_openlibs l
  result <- liftIO $ withCString "main" $ \name -> do
    (codePtr, codeLen) <- newCStringLen mudMainModule
    luaResult <- Lua.luaL_loadbuffer l codePtr (CSize $ fromIntegral codeLen) name
    free codePtr
    pure luaResult

  startTime <- liftIO $ getSystemTime
  startTimeTVar <- liftIO $ newTVarIO $ systemSeconds startTime
  deltaTimeAccumTVar <- liftIO $ newTVarIO 0

  let initSimulationState
        = SimulationState
        { _simulationStateOutChan = outChan
        , _simulationStateLuaInterpreter = lTVar
        , _simulationStateLastTime = startTimeTVar
        , _simulationStateDeltaTimeAccumMs = deltaTimeAccumTVar
        , _simulationStateTimeRate = 1 -- TODO: set this from configuration
        }
  if result /= Lua.LUA_OK
    then error "Error loading MUD code, aborting..."
    else runSimulation initSimulationState tick

tick :: MonadIO m => Simulation m ()
tick = do
  currTimeNano <- systemNanoseconds <$> liftIO getSystemTime
  let currTime = fromIntegral currTimeNano

  lastTimeTVar <- gets _simulationStateLastTime
  lastTime <- liftIO . atomically $ readTVar lastTimeTVar

  timeRate <- gets _simulationStateTimeRate
  accTimeTVar <- gets _simulationStateDeltaTimeAccumMs
  accTime <- liftIO . atomically . readTVar $ accTimeTVar

  let dt = currTime - lastTime `div` 1_000_000

  lTVar <- gets _simulationStateLuaInterpreter
  l <- liftIO . atomically $ readTVar lTVar

  when (accTime + dt > timeRate) $ do
    _ <- liftIO $ Lua.lua_pcall l (Lua.NumArgs 0) (Lua.NumResults 0) (Lua.StackIndex 0)
    resetAccTime accTimeTVar
  setLastTime lastTimeTVar currTime
  accumulateDt dt accTimeTVar

  tick
  where
    resetAccTime :: MonadIO m => TVar Int64 -> Simulation m ()
    resetAccTime accTimeTVar = liftIO . atomically $ writeTVar accTimeTVar 0

    setLastTime :: MonadIO m => TVar Int64 -> Int64 -> Simulation m ()
    setLastTime lastTimeTVar time = do
      liftIO . atomically . writeTVar lastTimeTVar $ time

    accumulateDt :: MonadIO m => Int64 -> TVar Int64 -> Simulation m ()
    accumulateDt dt accTimeTVar = do
      liftIO . atomically $ do
        modifyTVar' accTimeTVar $ \accTime -> accTime + dt
