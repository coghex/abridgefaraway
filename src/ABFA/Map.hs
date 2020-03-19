module ABFA.Map where
-- various list operations are defined to abstract away a 2D world

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put
import Data.List.Split ( chunksOf )
import ABFA.Data 

-- calculates the index of each biome
calcBiome :: Biome -> Int
calcBiome BNULL     = 0
calcBiome BSea      = 1
calcBiome BShallows = 2
calcBiome BDeeps    = 3
calcBiome BValley   = 4
calcBiome BCrags    = 5
calcBiome BPlains   = 6
calcBiome BFields   = 7
calcBiome BWastes   = 8
calcBiome BSteeps   = 9
calcBiome BPeaks    = 10

-- finds cardinal tiles of a specified tile
cardinals :: Int -> Int -> [a] -> ([a], [a], [a], [a])
cardinals zonew zoneh x = (nx, sx, ex, wx)
  where nx  = (drop zonew x) ++ (take zonew x)
        sx  = (drop ((zoneh-1)*zonew) x) ++ (take ((zoneh-1)*zonew) x)
        ext = (tail x)
        ex  = replaceEveryE zonew ext x
        wx  = replaceEveryW zonew x

-- finds cardinal tiles in a zone
zoneCardinals :: Int -> Int -> [a] -> [a] -> [a] -> [a] -> [a] -> ([a], [a], [a], [a])
zoneCardinals zonew zoneh nb sb eb wb x = (nx, sx, ex, wx)
  where nx  = nb ++ (drop zonew x)
        sx  = (take ((zoneh-1)*zonew) x) ++ sb
        ex  = replaceEveryE  zonew eb x
        wx  = replaceEveryWG zonew wb x

-- these helper functions are for the cardinal functions
replaceEveryE :: Int -> [a] -> [a] -> [a]
replaceEveryE n [] b = []
replaceEveryE n s  b = (take (n-1) s) ++ [(head b)] ++ (replaceEveryE (n) (drop n s) (drop n b))

replaceEveryW :: Int -> [a] -> [a]
replaceEveryW n [] = []
replaceEveryW n s  = (s !! (n-1)) : (take (n-1) s) ++ (replaceEveryW (n) (drop n s))

replaceEveryWG :: Int -> [a] -> [a] -> [a]
replaceEveryWG n [] b = []
replaceEveryWG n s  b = [(head b)] ++ (take (n-1) s) ++ (replaceEveryWG (n) (drop n s) (tail b))

-- removes the border around a zone list
removeZoneBorder :: Int -> Int -> BS.ByteString -> BS.ByteString
removeZoneBorder zonew zoneh bs = bsr
  where bsr = listToBS ret 1
        inp = bsToList bs  1
        r0  = drop zonew inp -- remove top row
        r1  = take ((zoneh-1)*(zonew-1)) r0 -- bottom row
        r2  = dropEvery (zonew-1) 0 r1 -- left row
        r3  = dropEvery (zonew-1) (-(zonew-1)) r2 -- right row
        ret = r3

-- drops every nth element of a list
dropEvery :: Int -> Int -> [a] -> [a]
dropEvery _ _      [] = []
dropEvery n offset x  = (take (offset+n) x) ++ (tail (dropEvery n 0 x))

-- takes every nth element of a list
takeEvery :: Int -> Int -> [a] -> [a]
takeEvery _ _      [] = []
takeEvery n offset x  = (head (drop offset x)):(takeEvery n offset (drop n x))

-- the formula for an ellipse
distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 x3 y3 t = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y2)*(y1-y2)))
  100*p1*p2

zoneDistance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Float
zoneDistance zonew zoneh x1 y1 x y x2 y2 x3 y3 t = do
  let p1 = (((nx1-nx2)*(nx1-nx2)))+(((ny1-ny2)*(ny1-ny2)))
      p2 = (((nx1-nx3)*(nx1-nx3)))+(((ny1-ny3)*(ny1-ny3)))
  100*p1*p2
  where nx1 = (fromIntegral(x1))+(fromIntegral(x)/fromIntegral(zonew))
        ny1 = (fromIntegral(y1))+(fromIntegral(y)/fromIntegral(zoneh))
        nx2 = (fromIntegral(x2))
        ny2 = (fromIntegral(y2))
        nx3 = (fromIntegral(x3))
        ny3 = (fromIntegral(y3))

-- these functions zip together a (x,y) value with the
-- grid, so that map functions are easy to write
expandGrid :: Int -> Int -> [a] -> [([(a, Int)], Int)]
expandGrid gridw gridh m = zip (map (workRows gridw) (chunksOf gridw m)) [0..gridh]

workRows :: Int -> [a] -> [(a, Int)]
workRows gridw l = do
  zip l [0..gridw]

flattenGrid :: [[a]] -> [a]
flattenGrid xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

stripGrid :: [([(a, Int)], Int)] -> [[a]]
stripGrid ((a, b):ys) = (stripRow a) : stripGrid ys
stripGrid _           = [[]]

stripRow :: [(a, Int)] -> [a]
stripRow ((a, b):ys) = a : stripRow ys
stripRow _           = []

-- this works for zones too
expandZone :: Int -> Int -> [a] -> [([(a, Int)], Int)]
expandZone zonew zoneh m = zip (map (workZoneRows zonew) (chunksOf zonew m)) [0..zoneh]

workZoneRows :: Int -> [a] -> [(a, Int)]
workZoneRows zonew l = do
  zip l [0..zonew]

-- returns the zone element from a 1-D list
tapZoneGridM :: Int -> Int -> Int -> [a] -> a
tapZoneGridM zonew x y zg = zg !! (x + (y*zonew))

-- converts bytestring to zone grid
bsToList :: BS.ByteString -> Int -> [Int]
bsToList bs n = map (fromInteger . toInteger) (BS.unpack bs)

-- converts zone grid to bytestring
listToBS :: [Int] -> Int -> BS.ByteString
listToBS l 2 = runPut $ sequence_ $ do
  map (putWord16le . fromIntegral) l
listToBS l n = runPut $ sequence_ $ do
  map (putWord8 . fromIntegral) l

-- does some basic subtraction for tuples
tupleSub :: (Num a) => (a, a) -> (a, a) -> (a, a)
tupleSub (ax, ay) (bx, by) = ((ax-bx), (ay-by))

-- adds the right movement in the right direction for the zone camera
moveCam :: Direction -> Float -> (Float, Float, Int) -> (Float, Float, Int)
moveCam DLeft  step (cx, cy, cz) = ((cx-step),        cy, cz)
moveCam DRight step (cx, cy, cz) = ((cx+step),        cy, cz)
moveCam DUp    step (cx, cy, cz) = (       cx, (cy+step), cz)
moveCam DDown  step (cx, cy, cz) = (       cx, (cy-step), cz)
moveCam DNULL  step (cx, cy, cz) = (       cx,        cy, cz)
