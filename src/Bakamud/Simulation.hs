{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bakamud.Simulation where

import Bakamud.Server.Monad
import Bakamud.Server.State
import Bakamud.Simulation.Event
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Int
import Data.Time.Clock.System
import qualified Lua as Lua

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
  lTVar <- asks _serverStateLuaInterpreterState

  startTime <- liftIO $ getSystemTime
  startTimeTVar <- liftIO $ newTVarIO $ systemSeconds startTime
  deltaTimeAccumTVar <- liftIO $ newTVarIO 0

  outChan <- asks _serverStateSimulationOutChan

  let initSimulationState
        = SimulationState
        { _simulationStateOutChan = outChan
        , _simulationStateLuaInterpreter = lTVar
        , _simulationStateLastTime = startTimeTVar
        , _simulationStateDeltaTimeAccumMs = deltaTimeAccumTVar
        , _simulationStateTimeRate = 1 -- TODO: set this from configuration
        }
  runSimulation initSimulationState tick

tick :: MonadIO m => Simulation m ()
tick = do
  outChan <- gets _simulationStateOutChan
  currTimeNano <- systemNanoseconds <$> liftIO getSystemTime
  let currTime = fromIntegral currTimeNano

  lastTimeTVar <- gets _simulationStateLastTime
  lastTime <- liftIO . atomically $ readTVar lastTimeTVar

  accTimeTVar <- gets _simulationStateDeltaTimeAccumMs

  let dt = currTime - lastTime `div` 1_000_000

  resetAccTime accTimeTVar
  setLastTime lastTimeTVar currTime
  accumulateDt dt accTimeTVar

  liftIO . atomically $ do
    writeTChan outChan $ SomethingHappened "Testing outbox pattern"

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
