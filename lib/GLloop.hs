module GLloop where

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLFW as GLFW
import Data.Bits ( (.|.) )
import Control.Monad.Identity (runIdentity)

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ()) 

getPos :: Int -> (Int, Int)
getPos 0 = (0,0)
getPos x = ((90 `quot` x),(60 `mod` x))


drawTile :: [GLuint] -> Int -> GLfloat -> IO ()
drawTile texs x j = do
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view

  let (a,b) = getPos x
  glTranslatef (fromIntegral a) (fromIntegral b) (-100.0)
  glBindTexture gl_TEXTURE_2D (texs!!x)
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
  glFlush

drawScene :: [GLuint] -> [Int] -> GLFW.Window -> IO ()
drawScene texs mmap win = do
  resequence_ (zipWith (drawTile texs) mmap [0..])
