module GLloop where

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLFW as GLFW
import Data.Bits ( (.|.) )
import Control.Monad.Identity (runIdentity)
import World
import Data.List.Split

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ()) 

sceneSetup :: Int -> IO ()
sceneSetup x = do
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  --glLoadIdentity

getPos :: Int -> (Int, Int)
getPos 0 = (0,0)
getPos x = ((90 `quot` x),(60 `mod` x))

drawTile :: [GLuint] -> Int -> Int -> Int -> IO ()
drawTile texs y x t = do
  -- glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
  --                       .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view

  glTranslatef (fromIntegral x) (fromIntegral y) (-100.0)
  glBindTexture gl_TEXTURE_2D (texs!!t)
  glBegin gl_QUADS
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
  let mapexp = chunksOf 90 mmap
  let mapnew = zip (map workRows mapexp) [0..60]

  sceneSetup 0
  drawTile texs 0 0 2
  drawTile texs 0 10 1
  -- resequence_ (drawRow texs (getY mapnew) 3)
  glFlush
