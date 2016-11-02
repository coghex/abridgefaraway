module World where

import System.Random
import Language.Haskell.Interpreter
import Data.List.Split
import Data.Bits ( (.|.) )
import Control.Monad
import Graphics.GL
import Graphics.GLU

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

sceneSetup :: IO ()
sceneSetup = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT

drawTile :: [GLuint] -> Int -> Int -> Int -> IO ()
drawTile texs x y t = do
  glLoadIdentity
  glTranslatef ((fromIntegral x)) ((fromIntegral y)) (-100)

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
