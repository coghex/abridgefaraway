module Zone where

import Data.List.Split (chunksOf)
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
  | (i==j)    = ((buildThisZone a), j)
  | otherwise = (a, j)

buildThisZone :: [Int] -> [Int]
buildThisZone l = do
  let z0 = expandThisZone (take (256*256) (repeat 0))--l
      z1 = stripZone z0
      z2 = flattenZone z1
  z2

expandThisZone :: [Int] -> [([(Int, Int)], Int)]
expandThisZone m = zip (map workZoneRows (chunksOf zonew m)) [0..zoneh]

workZoneRows :: [Int] -> [(Int, Int)]
workZoneRows l = do
  zip l [0..zonew]

stripZone :: [([(Int, Int)], Int)] -> [[Int]]
stripZone ((a,b):ys) = (stripZoneRow a) : stripZone ys
stripZone _          = [[]]

stripZoneRow :: [(Int, Int)] -> [Int]
stripZoneRow ((a,b):ys) = a : stripZoneRow ys
stripZoneRow _          = []

flattenZone :: [[Int]] -> [Int]
flattenZone xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []
