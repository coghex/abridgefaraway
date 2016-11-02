module World where

import System.Random
import Data.List.Split
import Data.Bits ( (.|.) )
import Control.Monad
import Control.Monad.RWS.Strict ( get )
import Graphics.GL
import Graphics.GLU
import qualified Graphics.UI.GLFW as GLFW
import State

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

sceneSetup :: IO ()
sceneSetup = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT

workRows :: [Int] -> [(Int, Int)]
workRows l = do
  zip l [0..120]

expandMap :: [Int] -> [([(Int, Int)], Int)]
expandMap m = zip (map workRows (chunksOf 120 m)) [0..90]

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance tile a b x1 y1 x2 y2 = do
  let t = tile+1
  let p1 = ((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1))
  let p2 = ((x2-a)*(x2-a))+((y2-b)*(y2-b))
  p1*p2*t*t

drawWorld :: [GLuint] -> [([(Int, Int)], Int)] -> [IO()]
drawWorld texs m = map (drawRow texs) m

drawRow :: [GLuint] -> ([(Int, Int)], Int) -> IO ()
drawRow texs (a, b) = resequence_ (map (drawSpot texs b) a)

drawSpot :: [GLuint] -> Int -> (Int, Int) -> IO ()
drawSpot texs y (t,x) = (drawTile texs x y t)

drawTile :: [GLuint] -> Int -> Int -> Int -> IO ()
drawTile texs x y t = do
  glLoadIdentity
  glTranslatef ((fromIntegral x)-60.0) ((fromIntegral y)-45.0) (-200)

  glBindTexture GL_TEXTURE_2D (texs!!t)
  glBegin GL_QUADS
  glTexCoord2f   0    0
  glVertex3f   (-1) (-1)    1  -- bottom left of quad (Front)
  glTexCoord2f   1    0
  glVertex3f     1  (-1)    1  -- bottom right of quad (Front)
  glTexCoord2f   1    1 
  glVertex3f     1    1     1  -- top right of quad (Front)
  glTexCoord2f   0    1 
  glVertex3f   (-1)   1     1  -- top left of quad (Front)
  glTexCoord2f   1    0 
  glVertex3f   (-1) (-1) (-1)  -- bottom right of quad (Back)
  glTexCoord2f   1    1 
  glVertex3f   (-1)   1  (-1)  -- top right of quad (Back)
  glTexCoord2f   0    1 
  glVertex3f     1    1  (-1)  -- top left of quad (Back)
  glTexCoord2f   0    0 
  glVertex3f     1  (-1) (-1)  -- bottom left of quad (Back)
  
  glEnd

drawScene :: State -> GLFW.Window -> IO ()
drawScene state win = do
 -- state <- get
  let mapnew = expandMap (stateGrid state)
  resequence_ (drawWorld (stateTexs state) mapnew)
  glFlush

randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
randomList bnds n = take n . randomRs bnds

randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR(min, max)
