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

buildZone :: State -> Int -> Int -> [[Int]]
buildZone state x y = do
  let zones = stateZones state
  let i = zoneFlatIndex x y
  let z2 = zip zones [0..(gridh*gridw)]
  let z3 = map (changeZone i) z2
  let z4 = fst (unzip z3)
  z4

changeZone :: Int -> ([Int], Int) -> ([Int], Int)
changeZone i (a, j)
  | (i==j)    = ((take (256*256) (repeat 1)), j)
  | otherwise = (a, j)
