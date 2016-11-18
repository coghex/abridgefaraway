module Game.Road where

import Game.Zone

south = 1::Int
west = 2::Int
north = 3::Int
east = 4::Int
nfits1 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
sfits2 = [1, 2, 6, 8, 9, 21, 22, 40]
nfits2 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
nfits6 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
efits6 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
wfits7 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
efits7 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
wfits8 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
nfits8 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
sfits19 = [1, 2, 6, 8, 9, 21, 22, 40]
sfits36 = [1, 2, 6, 8, 9, 21, 22, 40]
efits36 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]

setZ :: Int -> Int -> Int -> [Int] -> [Int]
setZ n x y z = do
  let z0 = expandZone z
  let z1 = map (pathRow x y n) z0
  let z2 = stripZone z1
  flattenZone z2

initPath :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> [Int]
initPath x y dir t z []        = z
--------------------------------------------------------------
-- p1 - dead end south
--------------------------------------------------------------
initPath x y 0 0 z (1:rands) = initPath x (y+1) north 1 (setZ 1 x y z) rands
initPath x y 1 n z (1:rands) = if (n `elem` nfits1) then (setZ 1 x y z) else (initPath x y south n z rands)
--------------------------------------------------------------
-- p2 - up/down
--------------------------------------------------------------
initPath x y 0 0 z (2:rands) = do
  let z0 = setZ 2 x y z
  let z1 = initPath x (y+1) north 2 z0 rands
  initPath x (y-1) south 2 z1 rands
initPath x y 3 n z (2:rands) = if (n `elem` sfits2) then do
    let z0 = setZ 2 x y z
    initPath x (y+1) north 2 z0 rands
  else
    initPath x y north n z rands
initPath x y 1 n z (2:rands) = if (n `elem` nfits2) then do
    let z0 = setZ 2 x y z
    initPath x (y-1) south 2 z0 rands
  else
    initPath x y south n z rands
--------------------------------------------------------------
-- p6 - N->E
--------------------------------------------------------------
initPath x y 0 0 z (6:rands) = do
  let z0 = setZ 6 x y z
  let z1 = initPath x (y+1) north 6 z0 rands
  initPath (x+1) y east 6 z1 rands
initPath x y 1 n z (6:rands) = if (n `elem` efits6) then do
    let z0 = setZ 6 x y z
    initPath (x+1) y east 6 z0 rands
  else
    initPath x y south n z rands
initPath x y 2 n z (6:rands) = if (n `elem` nfits6) then do
    let z0 = setZ 6 x y z
    initPath x (y+1) north 6 z0 rands
  else
    initPath x y west n z rands
--------------------------------------------------------------
-- p7 - left/right
--------------------------------------------------------------
initPath x y 0 0 z (7:rands) = do
  let z0 = setZ 7 x y z
  let z1 = initPath (x-1) y west 7 z0 rands
  initPath (x+1) y east 7 z1 rands
initPath x y 2 n z (7:rands) = if (n `elem` efits7) then do
    let z0 = setZ 7 x y z
    initPath (x-1) y west 7 z0 rands
  else
    initPath x y west 7 z rands
initPath x y 4 n z (7:rands) = if (n `elem` wfits7) then do
    let z0 = setZ 7 x y z
    initPath (x+1) y east 7 z0 rands
  else
    initPath x y east 7 z rands
--------------------------------------------------------------
-- p8 - W->N
--------------------------------------------------------------
initPath x y 0 0 z (8:rands) = do
  let z0 = setZ 8 x y z
  let z1 = initPath (x-1) y west 8 z0 rands
  initPath x (y+1) north 8 z1 rands
initPath x y 1 n z (8:rands) = if (n `elem` nfits8) then do
    let z0 = setZ 8 x y z
    initPath (x-1) y west 8 z0 rands
  else
    initPath x y south 8 z rands
initPath x y 4 n z (8:rands) = if (n `elem` wfits8) then do
    let z0 = setZ 8 x y z
    initPath x (y+1) north 8 z0 rands
  else
    initPath x y east 8 z rands
--------------------------------------------------------------
-- p19 - dead end north
--------------------------------------------------------------
initPath x y 0 0 z (19:rands) = do
  let z0 = setZ 19 x y z
  initPath x (y-1) south 19 z0 rands
initPath x y 3 n z (19:rands) = if (n `elem` sfits19) then (setZ 19 x y z) else (initPath x y north n z rands)
--------------------------------------------------------------
-- p36 - S->E
--------------------------------------------------------------
initPath x y 0 0 z (36:rands) = do
  let z0 = setZ 36 x y z
  let z1 = initPath x (y-1) south 36 z0 rands
  initPath (x+1) y east 36 z1 rands
initPath x y 3 n z (36:rands) = if (n `elem` sfits36) then do
    let z0 = setZ 36 x y z
    initPath (x+1) y east 36 z0 rands
  else
    initPath x y north n z rands
initPath x y 2 n z (36:rands) = if (n `elem` efits36) then do
    let z0 = setZ 36 x y z
    initPath x (y-1) south 36 z0 rands
  else
    initPath x y west n z rands

--------------------------------------------------------------
initPath x y dir t z (r:rands) = do
  initPath x y dir t z rands

pathRow :: Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
pathRow x y t (a, b) = ((map (pathSpot x y t b) a), b)

pathSpot :: Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
pathSpot x y t j (a, i)
  | (x==i)&&(y==j) = (t, i)
  | otherwise      = (a, i)
