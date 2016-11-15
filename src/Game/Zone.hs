module Game.Zone where

import Data.List.Split (chunksOf)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import Game.Settings
import Game.State
import Game.Draw
import Game.Rand

setZone :: [Int] -> Int -> Int -> [Int]
setZone a x y = do
  let zmap = zip a [0..(gridh*gridw)]
  fst $ unzip $ map (zoneMapper (x + (gridw*y))) zmap

zoneMapper :: Int -> (Int, Int) -> (Int, Int)
zoneMapper a (b, c)
  | a==c      = (1, c)
  | otherwise = (b, c)

drawZone :: State -> [[GL.TextureObject]] -> IO ()
drawZone state texs = do
  let x = fst (stateCursor state) 
  let y = snd (stateCursor state)
  let znew = expandZone $ stateCurrentZ state
  resequence_ (map (drawZoneRow texs (getZoneType state x y)) znew)
  GL.flush
--let x = fst (stateCursor state) 
  --let y = snd (stateCursor state)
  --let t = getZoneType state x y
  --withTextures2D [((texs!!1)!!10)] $ drawZoneTile (texs!!1) 120 90 10
  --drawZoneSpot texs 0 0 10 t


drawZoneSpot :: [[GL.TextureObject]] -> Int -> Int -> (Int, Int) -> IO ()
drawZoneSpot texs c y (t, x)
  | (c==4)               = withTextures2D [((texs!!c)!!0)] $ drawZoneTile (texs!!c) (x) (y) t
  | (c >= 0) && (c < 4)  = withTextures2D [((texs!!c)!!t)] $ drawZoneTile (texs!!c) (x) (y) t
  | otherwise            = print "no tex"

getZoneType :: State -> Int -> Int -> Int
getZoneType state x y = (stateGrid state)!!(x+(gridw*y))

expandZone :: [Int] -> [([(Int, Int)], Int)]
expandZone m = zip (map workZRows (chunksOf zonew m)) [0..zoneh]

workZRows :: [Int] -> [(Int, Int)]
workZRows l = do
  zip l [0..zonew]

drawZoneRow :: [[GL.TextureObject]] -> Int -> ([(Int, Int)], Int) -> IO ()
drawZoneRow texs c (a,b) = resequence_ (map (drawZoneSpot texs c b) a)
