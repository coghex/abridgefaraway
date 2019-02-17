module Game.Map where

import Numeric (showFFloat)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Split ( chunksOf )
import Data.Maybe ( fromMaybe )
import Game.Settings
import Game.Data

data Card = North | South | West | East

-- resequences IO events
resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

-- calculates the required zoom at the world map
theZoom :: Int
theZoom = max (quot (4192*gridh) screenh) (quot (4192*gridw) screenw)

-- returns a cardinal list of elements surrounding each element
cardinals :: [a] -> ([a], [a], [a], [a])
cardinals x = do
  let nx  = (drop gridw x) ++ (take gridw x)
      sx  = (drop ((gridh-1)*gridw) x) ++ (take ((gridh-1)*gridw) x)
      ext = (tail x)
      ex  = replaceEveryE gridw ext x
      wx  = replaceEveryW gridw x
  (nx, sx, ex, wx)

zoneCardinals :: [a] -> ([a], [a], [a], [a])
zoneCardinals x = do
  let nx  = (drop zonew x) ++ (take zonew x)
      sx  = (drop ((zoneh-1)*zonew) x) ++ (take ((zoneh-1)*zonew) x)
      ext = (tail x)
      ex  = replaceEveryE zonew ext x
      wx  = replaceEveryW zonew x
  (nx, sx, ex, wx)

zoneCardinalsC :: [Int] -> ([Int], [Int], [Int], [Int])
zoneCardinalsC x = do
  let nx  = (drop zonew x) ++ (drop ((zoneh-1)*zonew) x)
      sx  = (take (zonew) x) ++ (take ((zoneh-1)*zonew) x)
      ext = (tail x)
      ex  = replaceEveryEC zonew x
      wx  = replaceEveryWC zonew x
  (nx, sx, ex, wx)

zoneCardinalsG :: [Int] -> Int -> ([Int], [Int], [Int], [Int])
zoneCardinalsG x n = do
  let nx  = (drop zonew x) ++ (take zonew (repeat n))
      sx  = (take zonew (repeat n)) ++ (take ((zoneh-1)*zonew) x)
      ext = (tail x)
      ex  = replaceEveryEG 56 zonew x
      wx  = replaceEveryWG 56 zonew x
  (nx, sx, ex, wx)

zoneCardinalsE :: [Float] -> ([Float], [Float], [Float], [Float])
zoneCardinalsE x = do
  let nx  = (drop zonew x) ++ (drop ((zoneh-1)*zonew) x)
      sx  = (take (zonew) x) ++ (take ((zoneh-1)*zonew) x)
      ext = (tail x)
      ex  = replaceEveryEC zonew x
      wx  = replaceEveryWC zonew x
  (nx, sx, ex, wx)

cardinalsXY :: Int -> Int -> [a] -> (a, a, a, a)
cardinalsXY x y l = cardMap (tapGridM x y) (cardinals l)

cardMap :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
cardMap f (a1, a2, a3, a4) = (f a1, f a2, f a3, f a4)

zoneCardinalsXY :: Int -> Int -> [a] -> (a, a, a, a)
zoneCardinalsXY x y l = cardMap (tapZoneGridM x y) (zoneCardinals l)

replaceEveryW :: Int -> [a] -> [a]
replaceEveryW n [] = []
replaceEveryW n s  = (s !! (n-1)) : (take (n-1) s) ++ (replaceEveryW (n) (drop n s))

replaceEveryE :: Int -> [a] -> [a] -> [a]
replaceEveryE n [] b = []
replaceEveryE n s  b = (take (n-1) s) ++ [(head b)] ++ (replaceEveryE (n) (drop n s) (drop n b))

replaceEveryWC :: Int -> [a] -> [a]
replaceEveryWC n [] = []
replaceEveryWC n s  = c : (take (n-1) s) ++ (replaceEveryWC (n) (drop n s))
  where c = head s

replaceEveryEC :: Int -> [a] -> [a]
replaceEveryEC n [] = []
replaceEveryEC n s  = (take (n-1) (tail s)) ++ [c] ++ (replaceEveryEC (n) (drop n s))
  where c = s !! (n-1)

replaceEveryWG :: a -> Int -> [a] -> [a]
replaceEveryWG c n [] = []
replaceEveryWG c n s  = c : (take (n-1) s) ++ (replaceEveryWG c (n) (drop n s))

replaceEveryEG :: a -> Int -> [a] -> [a]
replaceEveryEG c n [] = []
replaceEveryEG c n s = (take (n-1) s) ++ [c] ++ (replaceEveryEG c (n) (drop n s))

yList :: [Int]
yList = makeYList gridw [1..gridh]

makeYList :: Int -> [Int] -> [Int]
makeYList _ []      = []
makeYList 0 _       = []
makeYList n (x:xs) = (take gridw (repeat x)) ++ (makeYList (n-1) xs)

-- checks the bounds of the cursor, doesnt move the cursor if at the edge
moveCursor :: Int -> (Int, Int) -> Card -> (Int, Int)
moveCursor n (x, y) North
  | (y < gridh-n) = (x, y+n)
  | otherwise     = (x, y)
moveCursor n (x, y) South
  | (y > n-1)     = (x, y-n)
  | otherwise     = (x, y)
moveCursor n (x, y) West
  | (x > n-1)     = (x-n, y)
  | otherwise     = (x, y)
moveCursor n (x, y) East
  | (x < gridw-n) = (x+n, y)
  | otherwise     = (x, y)

tapGrid :: [a] -> Int -> Int -> a
tapGrid g x y = g !! (x + (y*gridw))

tapGridM :: Int -> Int -> [a] -> a
tapGridM x y g = g !! (x + (y*gridw))

tapZoneGridM :: Int -> Int -> [a] -> a
tapZoneGridM x y zg = zg !! (x + (y*zonew))

tapZoneGrid :: [a] -> Int -> Int -> a
tapZoneGrid g x y = g !! (x + (y*zonew))

parZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
parZipWith f xs ys = parMap rpar (uncurry f) (zip xs ys)

fdiv :: Int -> Int -> Float
fdiv x y = (fromIntegral x) / (fromIntegral y)

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 x3 y3 t = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  100*p1*p2

zoneDistance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Float -> Float
zoneDistance x1 y1 x y x2 y2 x3 y3 t = do
  let p1 = (((nx1-nx2)*(nx1-nx2)))+((((ny1-ny2)*(ny1-ny2))))
      p2 = (((nx1-nx3)*(nx1-nx3)))+((((ny1-ny3)*(ny1-ny3))))
  100*p1*p2
  where nx1 = (fromIntegral(x1))+(fromIntegral(x)/fromIntegral(zonew))
        ny1 = (fromIntegral(y1))+(fromIntegral(y)/fromIntegral(zoneh))
        nx2 = (fromIntegral(x2))
        ny2 = (fromIntegral(y2))
        nx3 = (fromIntegral(x3))
        ny3 = (fromIntegral(y3))
        
zoneLinearDistance :: (Float, Float) -> (Float, Float) -> Float
zoneLinearDistance (srcx, srcy) (dstx, dsty) = sqrt (((dstx-srcx)*(dstx-srcx)) + ((dsty-srcy)*(dsty-srcy)))

moveDirection :: (Float, Float) -> (Float, Float) -> Int -> Int
moveDirection (srcx, srcy) (dstx, dsty) prev
  | (dstx-srcx > dirfudge)  && (dsty-srcy > dirfudge)                            = 9
  | (dstx-srcx < -dirfudge) && (dsty-srcy > dirfudge)                            = 10
  | (dstx-srcx > dirfudge)  && (dsty-srcy < -dirfudge)                           = 11
  | (dstx-srcx < -dirfudge) && (dsty-srcy < -dirfudge)                           = 12
  | (dsty-srcy > dirfudge)                                                       = 5
  | (dsty-srcy < -dirfudge)                                                      = 6
  | (dstx-srcx > dirfudge)                                                       = 7
  | (dstx-srcx < -dirfudge)                                                      = 8
  | ((prev == 5) || (prev == 1)) && (dist < 0.1)                                 = 1
  | ((prev == 6) || (prev == 2)) && (dist < 0.1)                                 = 2
  | ((prev == 7) || (prev == 9)  || (prev == 11) || (prev == 3)) && (dist < 0.1) = 3
  | ((prev == 8) || (prev == 10) || (prev == 12) || (prev == 4)) && (dist < 0.1) = 4
  | otherwise                                                                    = prev
  where dirfudge = 1
        dist     = zoneLinearDistance (srcx, srcy) (dstx, dsty)

showXYZ :: (String, String, String) -> String
showXYZ (a, b, c) = "X:" ++ a ++ "Y:" ++ b ++ "Z:" ++ c

showFloatFoReal :: Float -> String
showFloatFoReal x = showFFloat (Just precision) x " "

mapXYZ :: (a->b) -> (a, a, a) -> (b, b, b)
mapXYZ f (a1, a2, a3) = (f a1, f a2, f a3)

roundTo :: Int -> Float -> Float
roundTo n x
  | abs(x) < (10^^(-n)) = 0.0
  | otherwise           = (fromInteger $ round $ x * (10^n)) / (10^^n)

expandZone :: [a] -> [([(a, Int)], Int)]
expandZone m = zip (map workZoneRows (chunksOf zonew m)) [0..zoneh]

workZoneRows :: [a] -> [(a, Int)]
workZoneRows l = do
  zip l [0..zonew]

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

tupMap :: (a -> b) -> [(a, a)] -> [(b, a)]
tupMap f s = map (mapTup f) s

mapTup :: (a -> b) -> (a, a) -> (b, a)
mapTup f (r, s) = (f r, s)

crossProduct :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
crossProduct (a1, a2, a3) (b1, b2, b3) = ((a2*b3 - a3*b2), (a3*b1 - a1*b3), (a1*b2 - a2-b1))

vmin :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
vmin (a1, a2, a3) (b1, b2, b3) = ((a1-b1), (a2-b2), (a3-b3))





