module Game.Time where

import Data.Time.Clock
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import Game.World
import Game.State
import Game.Sun

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
  gameTime env firststate n tsnew

formatTime :: Integer -> String
formatTime time = do
  let dq = quot time 36000
      dr = mod time 36000
      hq = quot dr 3600
      hr = mod dr 3600
      m  = quot hr 60
      s  = mod hr 60
  "day " ++ (show dq) ++ ": " ++ (show hq) ++ ":" ++ (show m) ++ ":" ++ (show s)

