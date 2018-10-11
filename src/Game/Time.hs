module Game.Time where

import Data.Time.Clock
import Control.Concurrent
import Control.Concurrent.Chan

import Game.State

gameTime :: Chan Integer -> Integer -> Int -> IO ()
gameTime chan time n = do
  start <- getCurrentTime
  writeChan chan time
  end <- getCurrentTime
  let diff = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) :: Int
      delay = n*1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  gameTime chan (time+1) n

formatTime :: Integer -> String
formatTime time = do
  let dq = quot time 36000
      dr = mod time 36000
      hq = quot dr 3600
      hr = mod dr 3600
      m  = quot hr 60
      s  = mod hr 60
  "day " ++ (show dq) ++ ": " ++ (show hq) ++ ":" ++ (show m) ++ ":" ++ (show s)
