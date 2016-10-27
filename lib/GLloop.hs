module GLloop where

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLFW as GLFW
import Data.Array.Repa hiding((++))
import Data.Bits ( (.|.) )

drawScene :: [GLuint] -> Array U DIM2 Int -> GLFW.Window -> IO()
drawScene texs map win = do
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
