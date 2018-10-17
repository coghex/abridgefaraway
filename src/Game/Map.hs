module Game.Map where

import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Split ( chunksOf )
import Game.Settings

data Card = North | South | West | East

-- returns a cardinal list of elements surrounding each element
cardinals :: [a] -> ([a], [a], [a], [a])
cardinals x = do
  let nx  = (drop gridw x) ++ (take gridw x)
      sx  = (drop ((gridh-1)*gridw) x) ++ (take ((gridh-1)*gridw) x)
      ext = (tail x)
      ex  = replaceEveryE gridw ext x
      wx  = replaceEveryW gridw x
  (nx, sx, ex, wx)

replaceEveryW :: Int -> [a] -> [a]
replaceEveryW n [] = []
replaceEveryW n s  = (s !! (n-1)) : (take (n-1) s) ++ (replaceEveryW (n) (drop n s))

replaceEveryE :: Int -> [a] -> [a] -> [a]
replaceEveryE n [] b = []
replaceEveryE n s  b = (take (n-1) s) ++ [(head b)] ++ (replaceEveryE (n) (drop n s) (drop n b))

-- checks the bounds of the cursor, doesnt move the cursor if at the edge
moveCursor :: Int -> (Int, Int) -> Card -> (Int, Int)
moveCursor n (x, y) North
  | (y < gridh-n) = (x, y+n)
  | otherwise     = (x, y)
moveCursor n (x, y) South
  | (y > n-1)       = (x, y-n)
  | otherwise     = (x, y)
moveCursor n (x, y) West
  | (x > n-1)       = (x-n, y)
  | otherwise     = (x, y)
moveCursor n (x, y) East
  | (x < gridw-n) = (x+n, y)
  | otherwise     = (x, y)

tapGrid :: [a] -> Int -> Int -> a
tapGrid g x y = g !! (x + (y*gridw))

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
