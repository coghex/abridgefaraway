module Game.Road where

import Game.Zone

south = 1::Int
west = 2::Int
north = 3::Int
east = 4::Int
nfits1 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
sfits2 = [1, 2, 6, 8, 9, 21, 22, 40]
nfits2 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
sfits3 = [1, 2, 6, 8, 9, 21, 22, 40]
nfits3 = [9, 23, 39]
efits3 = [4, 13, 33]
wfits4 = [3]
nfits4 = [10, 35, 40]
nfits6 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
efits6 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
wfits7 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
efits7 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
wfits8 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
nfits8 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
sfits9 = [3, 12, 15]
nfits9 = [2, 3, 16, 19, 21, 22, 33, 34, 36, 38]
efits9 = [10, 27]
sfits10 = [4, 16]
wfits10 = [9]
efits14 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
sfits19 = [1, 2, 6, 8, 9, 21, 22, 40]
wfits20 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]
sfits36 = [1, 2, 6, 8, 9, 21, 22, 40]
efits36 = [7, 8, 18, 20, 23, 24, 25, 30, 31, 37, 38]
sfits38 = [1, 2, 6, 8, 9, 21, 22, 40]
wfits38 = [6, 7, 14, 18, 24, 27, 28, 30, 31, 35, 36, 37]

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
-- p3
--------------------------------------------------------------
initPath x y 0 0 z (3:rands) = do
  let z0 = setZ 3 x y z
  let z1 = initPath x (y-1) south 3 z0 rands
  let z2 = initPath x (y+1) north 3 z1 rands
  initPath (x+1) y east 3 z2 rands
initPath x y 3 n z (3:rands) = if (n `elem` sfits3) then do
    let z0 = setZ 3 x y z
    let z1 = initPath x (y+1) north 3 z0 rands
    initPath (x+1) y east 3 z0 rands
  else
    initPath x y north n z rands
initPath x y 1 n z (3:rands) = if (n `elem` nfits3) then do
    let z0 = setZ 3 x y z
    let z1 = initPath x (y-1) south 3 z0 rands
    initPath (x+1) y east 3 z1 rands
  else
    initPath x y south n z rands
initPath x y 2 n z (3:rands) = if (n `elem` efits3) then do
    let z0 = setZ 3 x y z
    let z1 = initPath x (y-1) south 3 z0 rands
    initPath x (y+1) north 3 z1 rands
  else
    initPath x y west n z rands
--------------------------------------------------------------
-- p4
--------------------------------------------------------------
initPath x y 0 0 z (4:rands) = do
  let z0 = setZ 4 x y z
  let z1 = initPath (x-1) y west 4 z0 rands
  initPath x (y+1) north 4 z1 rands
initPath x y 4 n z (4:rands) = if (n `elem` wfits4) then do
    let z0 = setZ 4 x y z
    initPath x (y+1) north 4 z0 rands
  else
    initPath x y east n z rands
initPath x y 1 n z (4:rands) = if (n `elem` nfits4) then do
    let z0 = setZ 4 x y z
    initPath (x-1) y west 4 z0 rands
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
-- p9
--------------------------------------------------------------
initPath x y 0 0 z (9:rands) = do
  let z0 = setZ 9 x y z
  let z1 = initPath x (y-1) south 9 z0 rands
  let z2 = initPath x (y+1) north 9 z1 rands
  initPath (x+1) y east 9 z2 rands
initPath x y 1 n z (9:rands) = if (n `elem` nfits9) then do
    let z0 = setZ 9 x y z
    let z1 = initPath x (y-1) south 9 z0 rands
    initPath (x+1) y east 9 z1 rands
  else
    initPath x y south n z rands
initPath x y 2 n z (9:rands) = if (n `elem` efits9) then do
    let z0 = setZ 9 x y z
    let z1 = initPath x (y-1) south 9 z0 rands
    initPath x (y+1) north 9 z1 rands
  else
    initPath x y west n z rands
initPath x y 3 n z (9:rands) = if (n `elem` sfits9) then do
    let z0 = setZ 9 x y z 
    let z1 = initPath x (y+1) north 9 z0 rands
    initPath (x+1) y east 9 z1 rands
  else
    initPath x y north n z rands
--------------------------------------------------------------
-- p10
--------------------------------------------------------------
--
--------------------------------------------------------------
-- p14 - dead end west
--------------------------------------------------------------
initPath x y 0 0 z (14:rands) = do
  let z0 = setZ 14 x y z
  initPath (x+1) y east 14 z0 rands
initPath x y 2 n z (14:rands) = if (n `elem` efits14) then (setZ 14 x y z) else (initPath x y west n z rands)
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
--------------------------------------------------------------
initPath x y dir t z (r:rands) = do
  initPath x y dir t z rands

pathRow :: Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
pathRow x y t (a, b) = ((map (pathSpot x y t b) a), b)

pathSpot :: Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
pathSpot x y t j (a, i)
  | (x==i)&&(y==j) = (t, i)
  | otherwise      = (a, i)
