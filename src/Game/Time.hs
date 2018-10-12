module Game.Time where

import Data.Time.Clock
import Control.Concurrent
import Control.Concurrent.Chan

import Game.State
import Game.Sun

gameTime :: Chan Integer -> Chan Sun -> Sun -> Integer -> Int -> IO ()
gameTime chan sunchan sun time n = do
  start <- getCurrentTime
  writeChan chan time
  let newsun = moveSun sun time
  writeChan sunchan newsun
  end <- getCurrentTime
  let diff = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) :: Int
      delay = n*1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  gameTime chan sunchan newsun (time+1) n

formatTime :: Integer -> String
formatTime time = do
  let dq = quot time 36000
      dr = mod time 36000
      hq = quot dr 3600
      hr = mod dr 3600
      m  = quot hr 60
      s  = mod hr 60
  "day " ++ (show dq) ++ ": " ++ (show hq) ++ ":" ++ (show m) ++ ":" ++ (show s)
