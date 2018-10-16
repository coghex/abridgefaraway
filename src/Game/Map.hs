module Game.Map where

import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Split ( chunksOf )
import Game.Settings

parZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
parZipWith f xs ys = parMap rpar (uncurry f) (zip xs ys)

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 x3 y3 t = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  100*p1*p2

expandGrid :: [a] -> [([(a, Int)], Int)]
expandGrid m = zip (map workRows (chunksOf gridw m)) [0..gridh]

workRows :: [a] -> [(a, Int)]
workRows l = do
  zip l [0..gridw]

flattenGrid :: [[a]] -> [a]
flattenGrid xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

stripGrid :: [([(a, Int)], Int)] -> [[a]]
stripGrid ((a, b):ys) = (stripRow a) : stripGrid ys
stripGrid _           = [[]]

stripRow :: [(a, Int)] -> [a]
stripRow ((a, b):ys) = a : stripRow ys
stripRow _           = []
