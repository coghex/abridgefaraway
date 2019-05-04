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

-- converts bytestring to zone grid
bsToList :: BS.ByteString -> Int -> [Int]
bsToList bs n = map (fromInteger . toInteger) (BS.unpack bs)

-- converts zone grid to bytestring
listToBS :: [Int] -> Int -> BS.ByteString
listToBS l n = runPut $ sequence_ $ do
  map (putWord8 . fromIntegral) l

