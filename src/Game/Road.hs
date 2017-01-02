module Game.Road where

import Game.Zone

south = 1::Int
west = 2::Int
north = 3::Int
east = 4::Int
nfits1 = [2, 3, 16, 19, 21, 22, 25, 33, 34, 36, 38]
sfits2 = [1, 2, 6, 8, 9, 21, 22, 40]
nfits2 = [2, 3, 16, 19, 21, 22, 25, 33, 34, 36, 38]
sfits3 = [1, 2, 6, 8, 9, 21, 22, 40]
nfits3 = [9, 23, 39]
efits3 = [4, 13, 16, 33]
wfits4 = [3, 12, 15]
nfits4 = [10, 35]
nfits6 = [2, 3, 16, 19, 21, 22, 25, 33, 34, 36, 38]
efits6 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
wfits7 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
efits7 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
wfits8 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
nfits8 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
sfits9 = [3, 12, 15]
nfits9 = [2, 3, 16, 19, 21, 22, 25, 33, 34, 36, 38]
efits9 = [10, 27]
sfits10 = [4, 16]
wfits10 = [9]
efits14 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
nfits15 = [9, 23, 39]
efits15 = [16]
sfits16 = [1, 2, 6, 8, 9, 21, 22, 40]
wfits16 = [15]
nfits16 = [10, 40]
wfits18 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
efits18 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
sfits19 = [1, 2, 6, 8, 9, 21, 22, 40]
wfits20 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
sfits21 = [1, 2, 6, 8, 9, 21, 22, 40]
nfits21 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
sfits22 = [1, 2, 6, 8, 9, 21, 22, 40]
nfits22 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
wfits30 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
efits30 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
wfits31 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
efits31 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
sfits36 = [1, 2, 6, 8, 9, 21, 22, 40]
efits36 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
sfits38 = [1, 2, 6, 8, 9, 21, 22, 40]
wfits38 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
sfits39 = [3, 12, 15]
efits39 = [40]
sfits40 = [4, 16]
wfits40 = [39]
nfits40 = [2, 16, 19, 21, 22, 33, 34, 36, 38]

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
    initPath x y west n z rands
initPath x y 4 n z (7:rands) = if (n `elem` wfits7) then do
    let z0 = setZ 7 x y z
    initPath (x+1) y east 7 z0 rands
  else
    initPath x y east n z rands
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
    initPath x y south n z rands
initPath x y 4 n z (8:rands) = if (n `elem` wfits8) then do
    let z0 = setZ 8 x y z
    initPath x (y+1) north 8 z0 rands
  else
    initPath x y east n z rands
--------------------------------------------------------------
-- p14 - dead end west
--------------------------------------------------------------
initPath x y 0 0 z (14:rands) = do
  let z0 = setZ 14 x y z
  initPath (x+1) y east 14 z0 rands
initPath x y 2 n z (14:rands) = if (n `elem` efits14) then (setZ 14 x y z) else (initPath x y west n z rands)
--------------------------------------------------------------
-- p18 - left/right
--------------------------------------------------------------
initPath x y 0 0 z (18:rands) = do
  let z0 = setZ 18 x y z
  let z1 = initPath (x-1) y west 18 z0 rands
  initPath (x+1) y east 18 z1 rands
initPath x y 2 n z (18:rands) = if (n `elem` efits18) then do
    let z0 = setZ 18 x y z
    initPath (x-1) y west 18 z0 rands
  else
    initPath x y west n z rands
initPath x y 4 n z (18:rands) = if (n `elem` wfits18) then do
    let z0 = setZ 18 x y z
    initPath (x+1) y east 18 z0 rands
  else
    initPath x y east n z rands
--------------------------------------------------------------
-- p19 - dead end north
--------------------------------------------------------------
initPath x y 0 0 z (19:rands) = do
  let z0 = setZ 19 x y z
  initPath x (y-1) south 19 z0 rands
initPath x y 3 n z (19:rands) = if (n `elem` sfits19) then (setZ 19 x y z) else (initPath x y north n z rands)
--------------------------------------------------------------
-- p20 - dead end east
--------------------------------------------------------------
initPath x y 0 0 z (20:rands) = do
  let z0 = setZ 20 x y z
  initPath (x-1) y west 20 z0 rands
initPath x y 4 n z (20:rands) = if (n `elem` wfits20) then (setZ 20 x y z) else (initPath x y east n z rands)
--------------------------------------------------------------
-- p21 - up/down
--------------------------------------------------------------
initPath x y 0 0 z (21:rands) = do
  let z0 = setZ 21 x y z
  let z1 = initPath x (y+1) north 21 z0 rands
  initPath x (y-1) south 21 z1 rands
initPath x y 3 n z (21:rands) = if (n `elem` sfits21) then do
    let z0 = setZ 21 x y z
    initPath x (y+1) north 21 z0 rands
  else
    initPath x y north n z rands
initPath x y 1 n z (21:rands) = if (n `elem` nfits21) then do
    let z0 = setZ 21 x y z
    initPath x (y-1) south 21 z0 rands
  else
    initPath x y south n z rands
--------------------------------------------------------------
-- p22 - up/down
--------------------------------------------------------------
initPath x y 0 0 z (22:rands) = do
  let z0 = setZ 22 x y z
  let z1 = initPath x (y+1) north 22 z0 rands
  initPath x (y-1) south 22 z1 rands
initPath x y 3 n z (22:rands) = if (n `elem` sfits22) then do
    let z0 = setZ 22 x y z
    initPath x (y+1) north 22 z0 rands
  else
    initPath x y north n z rands
initPath x y 1 n z (22:rands) = if (n `elem` nfits22) then do
    let z0 = setZ 22 x y z
    initPath x (y-1) south 22 z0 rands
  else
    initPath x y south n z rands
--------------------------------------------------------------
-- p30 - left/right
--------------------------------------------------------------
initPath x y 0 0 z (30:rands) = do
  let z0 = setZ 30 x y z
  let z1 = initPath (x-1) y west 30 z0 rands
  initPath (x+1) y east 30 z1 rands
initPath x y 2 n z (30:rands) = if (n `elem` efits30) then do
    let z0 = setZ 30 x y z
    initPath (x-1) y west 30 z0 rands
  else
    initPath x y west n z rands
initPath x y 4 n z (30:rands) = if (n `elem` wfits30) then do
    let z0 = setZ 30 x y z
    initPath (x+1) y east 30 z0 rands
  else
    initPath x y east n z rands
--------------------------------------------------------------
-- p31 - left/right
--------------------------------------------------------------
initPath x y 0 0 z (31:rands) = do
  let z0 = setZ 31 x y z
  let z1 = initPath (x-1) y west 31 z0 rands
  initPath (x+1) y east 31 z1 rands
initPath x y 2 n z (31:rands) = if (n `elem` efits31) then do
    let z0 = setZ 31 x y z
    initPath (x-1) y west 31 z0 rands
  else
    initPath x y west n z rands
initPath x y 4 n z (31:rands) = if (n `elem` wfits31) then do
    let z0 = setZ 31 x y z
    initPath (x+1) y east 31 z0 rands
  else
    initPath x y east n z rands
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
-- p38 - S->W
--------------------------------------------------------------
initPath x y 0 0 z (38:rands) = do
  let z0 = setZ 38 x y z
  let z1 = initPath x (y-1) south 38 z0 rands
  initPath (x-1) y west 38 z1 rands
initPath x y 3 n z (38:rands) = if (n `elem` sfits38) then do
    let z0 = setZ 38 x y z
    initPath (x-1) y west 38 z0 rands
  else
    initPath x y north n z rands
initPath x y 4 n z (38:rands) = if (n `elem` wfits38) then do
    let z0 = setZ 38 x y z
    initPath x (y-1) south 38 z0 rands
  else
    initPath x y east n z rands
initPath x y dir t z (r:rands) = do
  initPath x y dir t z rands

pathRow :: Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
pathRow x y t (a, b) = ((map (pathSpot x y t b) a), b)

pathSpot :: Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
pathSpot x y t j (a, i)
--  | (a>15)||(a<10) = (0, i)
  | (x==i)&&(y==j) = (t, i)
  | otherwise      = (a, i)

roadMapper :: [(Int, Int)] -> [Int] -> [[Int]] -> [Int]
roadMapper []               z _        = z
roadMapper _                z []       = z
roadMapper ((r1x, r1y):r1s) z (r2:r2s) = do
  let z0 = initPath r1x r1y 0 0 z r2
  roadMapper r1s z0 r2s

fixPath :: [Int] -> [Int] -> [Int]
fixPath paths bushes = fst $ unzip $ map pathFixer $ zip paths bushes

pathFixer :: (Int, Int) -> (Int, Int)
pathFixer (p, b)
  | (b >= 10) && (b <= 15) = (p, b)
  | otherwise              = (0, b)

initDoorPaths :: [Int] -> [[Int]] -> [Int]
initDoorPaths m rands = do
  let x = take (128*128) (repeat 0)
  let zold = expandZone m
  let znew = map pathDoorRows zold
  foldl (pathAllDoorRows (rands)) x znew
  --initPath 64 64 0 0 (take (128*128) (repeat 0)) (rands!!1)

pathDoorRows :: ([(Int, Int)], Int) -> ([(Int, Int)], Int)
pathDoorRows (l, j) = ((map (pathDoorSpots j) l), j)

pathDoorSpots :: Int -> (Int, Int) -> (Int, Int)
pathDoorSpots j (x, i)
  | (x==25)   = (1, i)
  | otherwise = (0, i)

pathAllDoorRows :: [[Int]] -> [Int] -> ([(Int, Int)], Int) -> [Int]
pathAllDoorRows rands a (b, j) = foldl (pathAllDoorSpots rands j) a b

pathAllDoorSpots :: [[Int]] -> Int -> [Int] -> (Int, Int) -> [Int]
pathAllDoorSpots rands j a (b, i)
  | (b>0) = initPath i j 0 0 a (rands!!b)
  | otherwise = a



