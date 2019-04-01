module GLUtil.Util where
-- all of the interfaces to graphics are abstracted
-- away to allow for new graphics libs (eg vulkan)
-- in the future.

import Data.Bits ((.|.))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.GLU
import Graphics.UI.GLUT (($=))

-- clears gl scene
sceneSetup :: IO ()
sceneSetup = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT

-- a basic tile
drawSquare :: IO ()
drawSquare = do
  glBegin GL_QUADS
  glTexCoord2f   0    1
  glVertex3f   (-1) (-1)  1
  glTexCoord2f   1    1
  glVertex3f     1  (-1)  1
  glTexCoord2f   1    0
  glVertex3f     1    1   1
  glTexCoord2f   0    0
  glVertex3f   (-1)   1   1
  glEnd

-- used to print GL errors
getGLErrors :: IO ()
getGLErrors = do
  err <- GL.get GL.errors
  mapM_ print err
