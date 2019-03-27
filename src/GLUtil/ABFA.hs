module GLUtil.ABFA where
-- all of the interfaces to graphics are abstracted
-- away to allow for new graphics libs (eg vulkan)
-- in the future.

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW

-- these are just type synonyms
type Window           = GLFW.Window
type Error            = GLFW.Error
type MouseButton      = GLFW.MouseButton
type MouseButtonState = GLFW.MouseButtonState
type ModifierKeys     = GLFW.ModifierKeys
type CursorState      = GLFW.CursorState
type Key              = GLFW.Key
type KeyState         = GLFW.KeyState

-- function synonyms
setErrorCallback       :: Maybe GLFW.ErrorCallback -> IO ()
setErrorCallback       = GLFW.setErrorCallback
setKeyCallback         :: GLFW.Window -> Maybe GLFW.KeyCallback -> IO ()
setKeyCallback         = GLFW.setKeyCallback
setMouseButtonCallback :: GLFW.Window -> Maybe GLFW.MouseButtonCallback -> IO ()
setMouseButtonCallback = GLFW.setMouseButtonCallback
setWindowSizeCallback  :: GLFW.Window -> Maybe GLFW.WindowSizeCallback -> IO ()
setWindowSizeCallback  = GLFW.setWindowSizeCallback
setScrollCallback      :: GLFW.Window -> Maybe GLFW.ScrollCallback -> IO ()
setScrollCallback      = GLFW.setScrollCallback
swapInterval           :: Int -> IO ()
swapInterval           = GLFW.swapInterval

-- this will use GLFW to create a window and destroy it when closed
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h title f = do
  GLFW.setErrorCallback $ Just glfwErrorCallback
  True <- GLFW.init
  Just window <- GLFW.createWindow w h title Nothing Nothing
  GLFW.makeContextCurrent $ Just window
  f window
  GLFW.setErrorCallback $ Just glfwErrorCallback
  GLFW.destroyWindow window
  GLFW.terminate
  where
    glfwErrorCallback e s = putStrLn $ show e ++ " " ++ show s

-- initializes the GL window before the game monad
initWindow :: IO ()
initWindow = do
  let width  = 2560
      height = 1440
      pos    = GL.Position 0 0
      size   = GL.Size (fromIntegral width) (fromIntegral height)
      h      = fromIntegral height / fromIntegral width :: Double
  do
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GLU.perspective 45 (1/h) (0.1) 500
    GL.matrixMode GL.$= GL.Modelview 0
    GL.loadIdentity
