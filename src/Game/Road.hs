module Game.Road where

import Game.Zone

south = 1::Int
west = 2::Int
north = 3::Int
east = 4::Int
nfits1 = [2, 19]
sfits2 = [1, 2, 6, 8]
nfits2 = [2, 19]
nfits6 = [2, 19]
efits6 = [7, 8]
wfits7 = [6, 7, 14]
efits7 = [7, 8]
wfits8 = [6, 7, 14]
nfits8 = [2, 19]
efits14 = [7, 8]
sfits19 = [1, 2, 6, 8]

setZ :: Int -> Int -> Int -> [Int] -> [Int]
setZ n x y z = do
  let z0 = expandZone z
  let z1 = map (pathRow x y n) z0
  let z2 = stripZone z1
  flattenZone z2

initPath :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> [Int]
initPath x y dir t z []        = z
--------------------------------------------------------------
-- paths2
--------------------------------------------------------------
initPath x y 0   0 z (1:rands) = initPath x (y+1) 3 1 (setZ 1 x y z) rands
initPath x y 1   n z (1:rands) = if (n `elem` nfits1) then (setZ 1 x y z) else (initPath x y 1 n z rands)
--------------------------------------------------------------
-- paths3
--------------------------------------------------------------
initPath x y 0   0 z (2:rands) = do
  let z0 = setZ 2 x y z
  let z1 = initPath x (y+1) 3 2 z0 rands
  initPath x (y-1) 1 2 z1 rands
initPath x y 3   n z (2:rands) = if (n `elem` sfits2) then do
    let z0 = setZ 2 x y z
    initPath x (y+1) 3 2 z0 rands
  else
    initPath x y 3 n z rands
initPath x y 1   n z (2:rands) = if (n `elem` nfits2) then do
    let z0 = setZ 2 x y z
    initPath x (y-1) 1 2 z0 rands
  else
    initPath x y 1 n z rands
--------------------------------------------------------------
-- paths7
--------------------------------------------------------------
initPath x y 0   0 z (6:rands) = do
  let z0 = setZ 6 x y z
  let z1 = initPath x (y+1) 3 6 z0 rands
  initPath (x+1) y 4 6 z1 rands
initPath x y 2   n z (6:rands) = if (n `elem` efits6) then do
    let z0 = setZ 6 x y z
    initPath x (y+1) 3 6 z0 rands
  else
    initPath x y 2 n z rands
initPath x y 1   n z (6:rands) = if (n `elem` nfits6) then do
    let z0 = setZ 6 x y z
    initPath (x+1) y 4 6 z0 rands
  else
    initPath x y 2 n z rands
--------------------------------------------------------------
-- paths8
--------------------------------------------------------------
initPath x y 0   0 z (7:rands) = do
  let z0 = setZ 7 x y z
  let z1 = initPath (x-1) y 2 7 z0 rands
  initPath (x+1) y 4 7 z1 rands
initPath x y 2   n z (7:rands) = if (n `elem` efits7) then do
    let z0 = setZ 7 x y z
    initPath (x-1) y 2 7 z0 rands
  else
    initPath x y 2 n z rands
initPath x y 4   n z (7:rands) = if (n `elem` wfits7) then do
    let z0 = setZ 7 x y z
    initPath (x+1) y 4 7 z0 rands
  else
    initPath x y 2 n z rands
--------------------------------------------------------------
-- paths9
--------------------------------------------------------------
initPath x y 0   0 z (8:rands) = do
  let z0 = setZ 8 x y z
  let z1 = initPath (x-1) y 2 8 z0 rands
  initPath x (y+1) 3 8 z0 rands
initPath x y 4   n z (8:rands) = if (n `elem` wfits8) then do
    let z0 = setZ 8 x y z
    initPath x (y+1) 3 8 z0 rands
  else
    initPath x y 4 n z rands
initPath x y 1   n z (8:rands) = if (n `elem` nfits8) then do
    let z0 = setZ 8 x y z
    initPath (x-1) y 2 8 z0 rands
  else
    initPath x y 1 n z rands
--------------------------------------------------------------
-- paths15
--------------------------------------------------------------
initPath x y 0   0 z (14:rands) = do
  let z0 = setZ 14 x y z
  initPath (x+1) y 4 14 z0 rands
initPath x y 2   n z (14:rands) = if (n `elem` efits14) then do
    setZ 14 x y z
  else
    initPath x y 2 n z rands
--------------------------------------------------------------
-- paths20
--------------------------------------------------------------
initPath x y 0   0 z (19:rands) = do
  let z0 = setZ 19 x y z
  initPath x (y-1) 1 19 z0 rands
initPath x y 3   n z (19:rands) = if (n `elem` sfits19) then do
    setZ 19 x y z
  else
    initPath x y 2 n z rands
------------------------------------------------------------------------
initPath x y dir t z (r:rands) = do
  initPath x y dir t z rands

pathRow :: Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
pathRow x y t (a, b) = ((map (pathSpot x y t b) a), b)

pathSpot :: Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
pathSpot x y t j (a, i)
  | (x==i)&&(y==j) = (t, i)
  | otherwise      = (a, i)
