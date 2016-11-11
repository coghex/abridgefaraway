module Zone where

import Data.List.Split (chunksOf)
import State
import Settings
import World

initZones :: Int -> [[Int]]
initZones t = do
  let zone = take (gridw*gridh) $ repeat $ take (zonew*zoneh) $ repeat t
  zone

seedZone :: State -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedZone state x y t (a, j) = (map (seedZoneRow state x y j t) a, j)

seedZoneRow :: State -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedZoneRow state x y j t (a, i) = (t, i)

zoneFlatIndex :: Int -> Int -> Int
zoneFlatIndex x y = ((x `quot` zonew)+(y `mod` zoneh))

buildZone :: State -> Int -> Int -> Int -> [[Int]]
buildZone state x y t = do
  let zones = stateZones state
  let i = zoneFlatIndex x y
  let z2 = zip zones [0..(gridh*gridw)]
  let z3 = map (changeZone state x y i t) z2
  let z4 = fst (unzip z3)
  z4

changeZone :: State -> Int -> Int -> Int -> Int -> ([Int], Int) -> ([Int], Int)
changeZone state x y i t (a, j)
  | (i==j)    = ((buildThisZone state x y t a), j)
  | otherwise = (a, j)

buildThisZone :: State -> Int -> Int -> Int -> [Int] -> [Int]
buildThisZone state x y t l = do
  let z0 = expandThisZone l
      z1 = map (seedZone state x y t) z0
      z2 = stripZone z1
      z3 = flattenZone z2
  z3

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
