module DrawZone where

import Data.List.Split (chunksOf)
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Zone
import Settings
import World
import State
import Draw

drawZone :: State -> Int -> Int -> IO ()
drawZone state x y = do
  let zonenew = expandZone ((stateZones state) !! (zoneFlatIndex x y))
  resequence_ $ map (drawZoneRow (stateZoneTexs state)) zonenew
  glFlush

drawZoneRow :: [[GL.TextureObject]] -> ([(Int, Int)], Int) -> IO ()
drawZoneRow texs (a,b) = resequence_ (map (drawZoneSpot texs b) a)

drawZoneSpot :: [[GL.TextureObject]] -> Int -> (Int, Int) -> IO ()
drawZoneSpot texs y (t, x) = withTextures2D (texs!!t) $ drawZoneTile (texs !! t) x y t

drawZoneTile :: [GL.TextureObject] -> Int -> Int -> Int -> IO ()
drawZoneTile texs x y t = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral zonew)/2))) (2*((fromIntegral y) - ((fromIntegral zoneh)/2))) (-50)
  GL.textureBinding GL.Texture2D GL.$= Just (texs!!0)
  glColor3f 1 1 1
  drawSquare

expandZone :: [Int] -> [([(Int, Int)], Int)]
expandZone m = zip (map zoneWorkRows (chunksOf zonew m)) [0..zoneh]

zoneWorkRows :: [Int] -> [(Int, Int)]
zoneWorkRows l = do
  zip l [0..zonew]

