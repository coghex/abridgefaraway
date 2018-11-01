module Game.Time where

import Data.Time.Clock
import Control.Parallel (par, pseq)
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import Game.World
import Game.State
import Game.Sun
import Game.Settings

gameTime :: Env -> State -> Int -> TimerState -> IO ()
gameTime env state n TStart = do
  let statechan = envStateChan1 env
      timerchan = envTimerChan env
  start <- getCurrentTime
  atomically $ writeTChan statechan state
  --let newsun = moveSun sun time
  --writeChan sunchan newsun
  let newstate = nextState state env

  timerstate <- (atomically (tryReadTChan timerchan))
  tsnew <- case (timerstate) of
    Nothing  -> return TStart
    Just x   -> return x

  end <- getCurrentTime
  let diff = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) :: Int
      delay = n*1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  gameTime env newstate n tsnew
gameTime env state n TStop = do
  let statechan = envStateChan2 env
      timerchan = envTimerChan env
  tsnew      <- atomically $ readTChan timerchan
  firststate <- atomically $ readTChan statechan
  let newstate  = simTime history firststate env
  pseq newstate $ gameTime env newstate n tsnew

formatTime :: Integer -> String
formatTime time = do
  let dq = quot time 720
      dr = mod time 720
      h  = quot dr 60
      m  = mod dr 60
  "day " ++ (show dq) ++ ": " ++ (show h) ++ ":" ++ (show m)

simTime :: Int -> State -> Env -> State
simTime 0 state env = nextState state env
simTime n state env = simTime (n-1) newstate env
  where newstate = nextSimState state env

-- a parallel version that creates a bunch of overflow sparks
--simTime :: Int -> State -> Env -> State
--simTime 0 state env = nextState state env
--simTime n state env = par newstate (seq nmin1 (simTime nmin1 newstate env))
--  where newstate = nextState state env
--        nmin1    = (n-1)
