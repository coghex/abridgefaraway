module Game.Draw where

import Data.Bits ((.|.))
import Data.List.Split (chunksOf)
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import System.Random (StdGen, mkStdGen)
import qualified Graphics.UI.GLFW as GLFW

import Game.State
import Game.Rand
import Game.Settings
import Game.Sun

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

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
      sun    = Sun { x = (x oldsun) + fromInteger((quot time (quot 36000 (toInteger(gridw)))))
                   , y = (y oldsun) + (sunSeason time)
                   , z = (z oldsun)
                   , l = (l oldsun) }
      oldsun = stateSun state
      time   = stateTime state 
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
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glColor3f b b b
  drawSquare
  where 
    b = sunSpots sunspots x y

expandGrid :: [Int] -> [([(Int, Int)], Int)]
expandGrid m = zip (map workRows (chunksOf gridw m )) [0..gridh]

workRows :: [Int] -> [(Int, Int)]
workRows l = do
  zip l [0..gridw]
