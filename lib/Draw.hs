module Draw where

import Data.Bits ((.|.))
import Data.List.Split ( chunksOf )
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW

import State

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

expandMap :: [Int] -> [([(Int, Int)], Int)]
expandMap m = zip (map workRows (chunksOf 120 m)) [0..90]

workRows :: [Int] -> [(Int, Int)]
workRows l = do
  zip l [0..120]

sceneSetup :: IO ()
sceneSetup = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT

drawTile :: [GLuint] -> Int -> Int -> Int -> IO ()
drawTile texs x y t = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - 60.0)) (2*((fromIntegral y) - 45.0)) (-400)

  glBindTexture GL_TEXTURE_2D (texs !! t)
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

drawScene :: State -> GLFW.Window -> IO ()
drawScene state win = do
  let mapnew = expandMap (stateGrid state)
  resequence_ (drawWorld (stateTexs state) mapnew)
  glFlush

drawWorld :: [GLuint] -> [([(Int, Int)], Int)] -> [IO ()]
drawWorld texs m = map (drawRow texs) m

drawRow :: [GLuint] -> ([(Int, Int)], Int) -> IO ()
drawRow texs (a, b) = resequence_ (map (drawSpot texs b) a)

drawSpot :: [GLuint] -> Int -> (Int, Int) -> IO ()
drawSpot texs y (t,x) = (drawTile texs x y t)
