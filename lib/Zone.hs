module Zone where

import State
import Settings
import World

seedZone :: Int -> Int -> Int -> [[Int]]
seedZone x y t = do
  let zone = take (gridw*gridh) $ repeat $ take (zonew*zoneh) $ repeat t
  zone

zoneFlatIndex :: Int -> Int -> Int
zoneFlatIndex x y = ((x `quot` zonew)+(y `mod` zoneh))
