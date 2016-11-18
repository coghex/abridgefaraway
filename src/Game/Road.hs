module Game.Road where

import Game.Zone

setZ :: Int -> Int -> Int -> [Int] -> [Int]
setZ n x y z = do
  let z0 = expandZone z
  let z1 = map (pathRow x y n) z0
  let z2 = stripZone z1
  flattenZone z2

initPath :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> [Int]
initPath x y dir t z []        = z
-- paths2
--------------------------------------------------------------
initPath x y 0   0 z (1:rands) = initPath x (y+1) 3 1 (setZ 1 x y z) rands
initPath x y 1   2 z (1:rands) = setZ 1 x y z
initPath x y 1   3 z (1:rands) = setZ 1 x y z
initPath x y 1  19 z (1:rands) = setZ 1 x y z
initPath x y 1  21 z (1:rands) = setZ 1 x y z
initPath x y 1  22 z (1:rands) = setZ 1 x y z
initPath x y 1  25 z (1:rands) = setZ 1 x y z
initPath x y 1  33 z (1:rands) = setZ 1 x y z
initPath x y 1  34 z (1:rands) = setZ 1 x y z
initPath x y 1  36 z (1:rands) = setZ 1 x y z
initPath x y 1  38 z (1:rands) = setZ 1 x y z
--------------------------------------------------------------
-- paths3
--------------------------------------------------------------
initPath x y 0   0 z (2:rands) = do
  let z0 = setZ 2 x y z
  let z1 = initPath x (y+1) 3 2 z0 rands
  initPath x (y-1) 1 2 z1 rands
-----------------------------------------
initPath x y 3   1 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y+1) 3 2 z0 rands
-----------------------------------------
initPath x y 1   2 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
initPath x y 3   2 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y+1) 3 2 z0 rands
-----------------------------------------
initPath x y 1   3 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
-----------------------------------------
initPath x y 3   6 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y+1) 3 2 z0 rands
-----------------------------------------
initPath x y 3   8 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y+1) 3 2 z0 rands
-----------------------------------------
initPath x y 3   9 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y+1) 3 2 z0 rands
-----------------------------------------
initPath x y 1  16 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
-----------------------------------------
initPath x y 1  19 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
-----------------------------------------
initPath x y 1  21 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
initPath x y 3   21 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y+1) 3 2 z0 rands
-----------------------------------------
initPath x y 1  22 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
initPath x y 3  22 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y+1) 3 2 z0 rands
-----------------------------------------
initPath x y 1  27 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
-----------------------------------------
initPath x y 1  33 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
-----------------------------------------
initPath x y 1  34 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
-----------------------------------------
initPath x y 1  36 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
-----------------------------------------
initPath x y 1  38 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y-1) 1 2 z0 rands
-----------------------------------------
initPath x y 3  40 z (2:rands) = do
  let z0 = setZ 2 x y z
  initPath x (y+1) 3 2 z0 rands
--------------------------------------------------------------
-- paths4
--------------------------------------------------------------
initPath x y 0   0 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  let z2 = initPath x (y-1) 1 3 z1 rands
  initPath (x+1) y 4 3 z2 rands
-----------------------------------------
initPath x y 3   1 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
-----------------------------------------
initPath x y 3   2 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
-----------------------------------------
initPath x y 2   4 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath x (y-1) 1 3 z1 rands
-----------------------------------------
initPath x y 3   6 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
-----------------------------------------
initPath x y 3   8 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
-----------------------------------------
initPath x y 3   9 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
initPath x y 1   9 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y-1) 1 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
-----------------------------------------
initPath x y 2  13 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath x (y-1) 1 3 z1 rands
-----------------------------------------
initPath x y 2  16 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath x (y-1) 1 3 z1 rands
-----------------------------------------
initPath x y 3  21 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
-----------------------------------------
initPath x y 3  22 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
-----------------------------------------
initPath x y 1  23 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y-1) 1 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
-----------------------------------------
initPath x y 3  33 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y+1) 3 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
-----------------------------------------
initPath x y 1  39 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y-1) 1 3 z0 rands
  initPath (x+1) y 4 3 z1 rands
---------------------------------------------------------------
-- paths5
---------------------------------------------------------------
initPath x y 0   0 z (4:rands) = do
  let z0 = setZ 4 x y z
  let z1 = initPath (x-1) y 2 4 z0 rands
  initPath x (y+1) 3 4 z1 rands
initPath x y 4   3 z (4:rands) = do
  let z0 = setZ 4 x y z
  initPath (x-1) y 2 4 z0 rands
initPath x y 1  10 z (4:rands) = do
  let z0 = setZ 4 x y z
  initPath x (y+1) 3 4 z0 rands
-------------------------------------------------------------------
initPath x y dir t z (r:rands) = do
  initPath x y dir t z rands

pathRow :: Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
pathRow x y t (a, b) = ((map (pathSpot x y t b) a), b)

pathSpot :: Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
pathSpot x y t j (a, i)
  | (x==i)&&(y==j) = (t, i)
  | otherwise      = (a, i)
