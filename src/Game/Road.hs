module Game.Road where

import Game.Zone
initPath :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
initPath x y t z []        = z
initPath x y t z (2:rands) = do
  let z0 = expandZone z
  let z1 = map (pathRow x y 2) z0
  let z2 = stripZone z1
  let z3 = flattenZone z2
  let z4 = initPath x (y+1) t z3 rands
  initPath x (y-1) t z4 rands
initPath x y t z (r:rands) = do
  let z0 = expandZone z
  let z1 = map (pathRow x y r) z0
  let z2 = stripZone z1
  let z3 = flattenZone z2
  initPath x y t z3 rands

pathRow :: Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
pathRow x y t (a, b) = ((map (pathSpot x y t b) a), b)

pathSpot :: Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
pathSpot x y t j (a, i)
  | (x==i)&&(y==j) = (t, i)
  | otherwise      = (a, i)
