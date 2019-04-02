module ABFA.Time where
-- timers are defined, they will run as threads

import Data.Time.Clock
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import ABFA.State
import ABFA.Game
import ABFA.Settings
import ABFA.World

-- worldTime will handle world events, weather, time, and such
worldTime :: Env -> State -> Int -> TimerState -> IO ()
worldTime env state n TStop = do
  let statechan = envStateChan2 env
      worldchan = envWorldChan env
  tsnew      <- atomically $ readTChan worldchan
  firststate <- atomically $ readTChan statechan
  let newstate = simTime history firststate env

-- simTime will simulate time occuring before the world is displayed
-- wont be the same if you were to play through it, since the simulation
-- takes so much longer it skips many steps
simTime :: Int -> State -> Env -> State
simTime 0 state env = nextState state env
simTime n state env = simTime (n-1) newstate env
  where newstate = nextSimState state env
