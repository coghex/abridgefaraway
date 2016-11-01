module GLloop where

import Graphics.GL
import Graphics.GLU
import qualified Graphics.UI.GLFW as GLFW
import Data.Bits ( (.|.) )
import Control.Monad.Identity (runIdentity)
import World
import Data.List.Split
import Settings

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ()) 

sceneSetup :: Int -> IO ()
sceneSetup x = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT
  --glLoadIdentity

drawWorld :: [GLuint] -> [([(Int, Int)], Int)] -> [IO()]
drawWorld texs m = map (drawRow texs) m
  
drawRow :: [GLuint] -> ([(Int, Int)], Int) -> IO ()
--drawRow texs (a,59) = return ()
--drawRow texs (a,58) = return ()
drawRow texs (a,b)  = resequence_ (map (drawSpot texs b) a)

drawSpot :: [GLuint] -> Int -> (Int, Int) -> IO ()
drawSpot texs y  (t,x) = (drawTile texs x y t)

drawTile :: [GLuint] -> Int -> Int -> Int -> IO ()
drawTile texs x y t = do
  -- glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
  --                       .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view

  glTranslatef ((fromIntegral x)-60) ((fromIntegral y)-45) (-200.0)
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
  -- glFlush

drawScene :: [GLuint] -> [Int] -> GLFW.Window -> IO ()
drawScene texs mmap win = do
  let mapexp = chunksOf mapw mmap
  let mapnew = zip (map workRows mapexp) [0..maph]
  sceneSetup 0
  --drawTile texs 0 0 2
  --drawTile texs 0 2 1
  resequence_ (drawWorld texs mapnew)
  glFlush

