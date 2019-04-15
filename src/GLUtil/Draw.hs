module GLUtil.Draw where
-- the drawing of various screens is defined

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified GLUtil.ABFA as GLFW
import GLUtil.Textures
import ABFA.Game
import ABFA.State

drawWorld :: State -> Env -> IO ()
drawWorld state env = do
  glFlush
