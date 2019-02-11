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

cardinalsXY :: Int -> Int -> [a] -> (a, a, a, a)
cardinalsXY x y l = cardMap (tapGridM x y) (cardinals l)

cardMap :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
cardMap f (a1, a2, a3, a4) = (f a1, f a2, f a3, f a4)

replaceEveryW :: Int -> [a] -> [a]
replaceEveryW n [] = []
replaceEveryW n s  = (s !! (n-1)) : (take (n-1) s) ++ (replaceEveryW (n) (drop n s))

replaceEveryE :: Int -> [a] -> [a] -> [a]
replaceEveryE n [] b = []
replaceEveryE n s  b = (take (n-1) s) ++ [(head b)] ++ (replaceEveryE (n) (drop n s) (drop n b))

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

tapZoneGrid :: Int -> Int -> [a] -> a
tapZoneGrid x y zg = zg !! (x + (y*zonew))

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





