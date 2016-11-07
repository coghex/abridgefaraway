module Zone where

import State
import Settings
import World

initZones :: Int -> [[Int]]
initZones t = do
  let zone = take (gridw*gridh) $ repeat $ take (zonew*zoneh) $ repeat t
  zone

zoneFlatIndex :: Int -> Int -> Int
zoneFlatIndex x y = ((x `quot` zonew)+(y `mod` zoneh))

buildZones :: State -> Int -> Int -> [[Int]]
buildZones state x y = do
  let zones = stateZones state
  --map buildZone
  initZones 1
