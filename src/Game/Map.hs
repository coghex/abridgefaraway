module Game.Map where

import Data.List.Split ( chunksOf )
import Game.Settings

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 x3 y3 t = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  p1*p2

expandGrid :: [Int] -> [([(Int, Int)], Int)]
expandGrid m = zip (map workRows (chunksOf gridw m)) [0..gridh]

workRows :: [Int] -> [(Int, Int)]
workRows l = do
  zip l [0..gridw]

flattenGrid :: [[Int]] -> [Int]
flattenGrid xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

stripGrid :: [([(Int, Int)], Int)] -> [[Int]]
stripGrid ((a, b):ys) = (stripRow a) : stripGrid ys
stripGrid _           = [[]]

stripRow :: [(Int, Int)] -> [Int]
stripRow ((a, b):ys) = a : stripRow ys
stripRow _           = []