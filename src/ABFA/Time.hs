module ABFA.Time where
-- timers are defined, they will run as threads

import Data.Time.Clock

import ABFA.Event
import ABFA.State
import ABFA.Game
import ABFA.Settings
import ABFA.World
import ABFA.Data

-- worldTime handles world events, weather, time, and such
worldTime :: Env -> State -> Int -> TimerState -> IO ()
worldTime env state n TStop = do
  let statechan = envStateChan2 env
      timerchan = envWTimerChan env
      history   = settingHistory (stateSettings state)
  tsnew      <- atomically $ readChan timerchan
  firststate <- atomically $ readChan statechan
  let newstate = simTime history firststate env
  worldTime env newstate n tsnew
worldTime env state n TStart = do
  let statechan = envStateChan1 env
      timerchan = envWTimerChan env
  start <- getCurrentTime
  atomically $ writeChan statechan state
  let newstate = nextSimState state env 1
  timerstate <- atomically $ tryReadChan timerchan
  tsnew <- case (timerstate) of
    Nothing -> return TStart
    Just x  -> return x
  end <- getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) :: Int
      delay = n*1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  worldTime env newstate n tsnew

-- animTime handles animation of everything
animTime :: Env -> State -> Int -> TimerState -> IO ()
animTime env state n TStop = do
  let statechan = envStateChan4 env
      timerchan = envATimerChan env
  tsnew      <- atomically $ readChan timerchan
  firstunits <- atomically $ readChan statechan
  let newstate = animState state env
  animTime env newstate n tsnew
animTime env state n TStart = do
  let statechan = envStateChan3 env
      timerchan = envATimerChan env
  start <- getCurrentTime
  let newstate = animState state env
  atomically $ writeChan statechan newstate
  timerstate <- (atomically (tryReadChan timerchan))
  tsnew <- case (timerstate) of
    Nothing -> return TStart
    Just x  -> return x
  end <- getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) :: Int
      delay = n*1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  animTime env newstate n tsnew


-- will turn the time integer into a printable format
formatTime :: Integer -> String
formatTime time = do
  let dq = quot time 720
      dr = mod  time 720
      h  = quot dr   60
      m  = mod  dr   60
  "day " ++ (show dq) ++ ": " ++ (show h) ++ ":" ++ (showSec m)

-- adds a zero to the seconds less than 10
showSec :: Integer -> String
showSec t
  | t < 10    = "0" ++ (show t)
  | otherwise = (show t)

-- simTime will simulate time occuring before the world is displayed
-- wont be the same if you were to play through it, since the simulation
-- takes so much longer it skips many steps
simTime :: Int -> State -> Env -> State
simTime 0 state env = nextSimState state env 1
simTime n state env = simTime (n-1) newstate env
  where newstate = nextSimState state env 59
