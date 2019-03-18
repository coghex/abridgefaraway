module Game.Civ where

import Numeric.Noise.Perlin
import Data.List (zip4)
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import GLUtil.Textures
import Game.Settings
import Game.State
import Game.Data
import Game.Map
import Game.Sky
import Game.Noise
import Game.Draw

--this will draw the civilizations, with the desireability of the land as the color
drawCiv :: State -> [GL.TextureObject] -> IO ()
drawCiv state texs = do
  let dnew = expandGrid $ stateDesireability state
  resequence_ (map (drawCivRow texs) dnew)
  glFlush

--drawing the rows
drawCivRow :: [GL.TextureObject] -> ([(Int, Int)], Int) -> IO ()
drawCivRow texs (v, y) = resequence_ (map (drawCivSpot texs y) v)

--drawing the tiles with the null tex
drawCivSpot :: [GL.TextureObject] -> Int -> (Int, Int) -> IO ()
drawCivSpot texs y (d, x) = withTextures2D [(texs!!10)] $ drawCivTile texs x y d

--drawing the tile
drawCivTile :: [GL.TextureObject] -> Int -> Int -> Int -> IO ()
drawCivTile texs x y d = do
  glLoadIdentity
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  glColor3f df df df
  drawSquare
  where thiszoom = fromIntegral $ theZoom
        df       = (fromIntegral d)/10.0

--this will calculate the desireability of the various zones
calcDesireability :: State -> Env -> [Int]
calcDesireability state env = zipWith (calcZoneDes p) input ([1..(gridh*gridw)])
  where g     = (stateGrid state)
        e     = (stateElev state)
        t     = getAirTemps (stateSkies state)
        v     = (stateVolcanism state)
        input = zip4 g e t v
        perl  = (envSeeds env) !! 2
        p     = makePerlin perl 4 0.15 0.5

--calculates the desireability of a single zone
calcZoneDes :: Perlin -> (Int, Int, Float, Float) -> Int -> Int
calcZoneDes p (g, e, t, v) n = des
  where des    = scoreZone perl g e t v
        (i, j) = findZoneIndex n
        perl   = 1.0 + (getNoise i j p)

--calculates the desireability score
scoreZone :: Float -> Int -> Int -> Float -> Float -> Int
scoreZone p g e t v = score
  where score  = round $ (gscore * escore * tscore * vscore) * p
        gscore = scoreGrid g
        escore = scoreElev e
        tscore = scoreTemp t
        vscore = scoreVolc v

--calculates score based on the grid
scoreGrid :: Int -> Float
scoreGrid 2 = 0.9
scoreGrid 3 = 0.7
scoreGrid 4 = 0.4
scoreGrid 5 = 0.2
scoreGrid 6 = 0.1
scoreGrid _ = 0.0

--calculates score based on the elevation
scoreElev :: Int -> Float
scoreElev e
  | (ne <= 0)   = 0.0
  | (ne > ph)   = 0.0
  | otherwise   = ((ph - ne) / (ph/2))
  where ne = ((fromIntegral(e)) - sealevel)
        ph = peaklevel / 2.0

--calculates score based on how ideal the temperature is
scoreTemp :: Float -> Float
scoreTemp t = max 0 (10.0 - (abs (t - idealtemp)))

--calculates score based on how volcanic the region is
scoreVolc :: Float -> Float
scoreVolc v
  | (v > 0.7) = 0
  | otherwise = 1

--formats the desireability scores for printing
formatDesireability :: [Int] -> (Int, Int) -> String
formatDesireability ds (x, y) = getDes x y d
  where d = (ds !! (x+gridw*y))

getDes :: Int -> Int -> Int -> String
getDes x y d = "Desireability: " ++ (show d)

