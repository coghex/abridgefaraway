module Game.Draw where

import Data.Bits ((.|.))
import Data.List.Split (chunksOf)
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import System.Random (StdGen, mkStdGen)
import qualified Graphics.UI.GLFW as GLFW
import GLUtil.Textures

import Game.State
import Game.Rand
import Game.Settings
import Game.Sun
import Game.Map

drawSquare :: IO ()
drawSquare = do
  glBegin GL_QUADS
  glTexCoord2f    0    0
  glVertex3f    (-1) (-1)   1
  glTexCoord2f    1    0
  glVertex3f      1  (-1)   1
  glTexCoord2f    1    1
  glVertex3f      1    1    1
  glTexCoord2f    0    1
  glVertex3f    (-1)   1    1
  glEnd

drawCursor :: State -> [GL.TextureObject] -> IO ()
drawCursor state texs = do
  let sun = stateSunSpots state
  withTextures2D [(head texs)] $ drawSceneTile [(head texs)] (sun) (fst (stateCursor state)) (snd (stateCursor state)) 0

drawScene :: State -> [GL.TextureObject] -> IO ()
drawScene state texs = do
  let gnew   = expandGrid $ stateGrid state
      sunspots = stateSunSpots state
  resequence_ (map (drawSceneRow texs sunspots) gnew)
  glFlush

drawSceneRow :: [GL.TextureObject] -> [Float] -> ([(Int, Int)], Int) -> IO ()
drawSceneRow texs sunspots (a, b) = resequence_ (map (drawSceneSpot texs sunspots b) a)

drawSceneSpot :: [GL.TextureObject] -> [Float] -> Int -> (Int, Int) -> IO ()
drawSceneSpot texs sunspots y (t, x) = withTextures2D [(texs!!t)] $ drawSceneTile texs sunspots x y t

drawSceneTile :: [GL.TextureObject] -> [Float] -> Int -> Int -> Int -> IO ()
drawSceneTile texs sunspots x y t = do
  glLoadIdentity
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  glColor3f t1 t2 t3
  drawSquare
  where 
    t1 = 0.8*b*b + 0.2*b
    t2 = 0.9*b
    t3 = (log (b+1))
    b  = sunSpots sunspots x y
    thiszoom = fromIntegral $ theZoom
