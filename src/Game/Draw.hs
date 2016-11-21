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

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

drawSquare :: IO ()
drawSquare = do
  glBegin GL_QUADS
  glTexCoord2f   0    0
  glVertex3f   (-1) (-1)   1
  glTexCoord2f   1    0
  glVertex3f     1  (-1)   1
  glTexCoord2f   1    1
  glVertex3f     1    1    1
  glTexCoord2f   0    1
  glVertex3f   (-1)   1    1
  glEnd

drawCursor :: State -> [GL.TextureObject] -> IO ()
drawCursor state texs = do
  withTextures2D [(last texs)] $ drawSceneTile [(last texs)] (fst (stateCursor state)) (snd (stateCursor state)) 0


drawScene :: State -> [GL.TextureObject] -> IO ()
drawScene state texs = do
  let gnew = expandGrid $ stateGrid state
  resequence_ (map (drawSceneRow texs) gnew)
  glFlush

drawSceneRow :: [GL.TextureObject] -> ([(Int, Int)], Int) -> IO ()
drawSceneRow texs (a, b) = resequence_ (map (drawSceneSpot texs b) a)

drawSceneSpot :: [GL.TextureObject] -> Int -> (Int, Int) -> IO ()
drawSceneSpot texs y (t, x) = withTextures2D [(texs!!t)] $ drawSceneTile texs x y t

drawSceneTile :: [GL.TextureObject] -> Int -> Int -> Int -> IO ()
drawSceneTile texs x y t = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-500)
  --GL.textureBinding GL.Texture2D GL.$= Just (texs!!t)
  glColor3f 1 1 1
  drawSquare

drawZoneTile :: [GL.TextureObject] -> Int -> Int -> Int -> GL.GLfloat -> IO ()
drawZoneTile texs x y t depth = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral zonew)/2))) (2*((fromIntegral y) - ((fromIntegral zoneh)/2))) (-depth)
  glScalef (1.0) (-1.0) 1.0
  --GL.textureBinding GL.Texture2D GL.$= Just (texs!!t)
  glColor3f 1 1 1
  drawSquare

expandGrid :: [Int] -> [([(Int, Int)], Int)]
expandGrid m = zip (map workRows (chunksOf gridw m)) [0..gridh]

workRows :: [Int] -> [(Int, Int)]
workRows l = do
  zip l [0..gridw]
