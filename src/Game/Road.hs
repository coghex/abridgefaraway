module Game.Road where

import Game.Zone
initPath :: Int -> Int -> [Int] -> [Int]
initPath x y z = do
  let z0 = expandZone z
  let z1 = map (pathRow x y) z0
  let z2 = stripZone z1
  flattenZone z2

pathRow :: Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
pathRow x y (a, b) = ((map (pathSpot x y b) a), b)

pathSpot :: Int -> Int -> Int -> (Int, Int) -> (Int, Int)
pathSpot x y j (a, i)
  | (x==i)&&(y==j) = (1, i)
  | otherwise      = (0, i)
