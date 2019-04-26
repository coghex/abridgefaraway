module ABFA.Map where
-- various list operations are defined to abstract away a 2D world

import Data.List.Split ( chunksOf )
import ABFA.Data 

-- the formula for an ellipse
distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 x3 y3 t = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y2)*(y1-y2)))
  100*p1*p2

-- these functions zip together a (x,y) value with the
-- grid, so that map functions are easy to write
expandGrid :: Int -> Int -> [a] -> [([(a, Int)], Int)]
expandGrid gridw gridh m = zip (map (workRows gridw) (chunksOf gridw m)) [0..gridh]

workRows :: Int -> [a] -> [(a, Int)]
workRows gridw l = do
  zip l [0..gridw]

flattenGrid :: [[a]] -> [a]
flattenGrid xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

stripGrid :: [([(a, Int)], Int)] -> [[a]]
stripGrid ((a, b):ys) = (stripRow a) : stripGrid ys
stripGrid _           = [[]]

stripRow :: [(a, Int)] -> [a]
stripRow ((a, b):ys) = a : stripRow ys
stripRow _           = []
