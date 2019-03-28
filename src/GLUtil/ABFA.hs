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
swapBuffers            :: Window -> IO ()
swapBuffers            = GLFW.swapBuffers
pollGLFWEvents         :: IO ()
pollGLFWEvents         = GLFW.pollEvents
windowShouldClose      :: Window -> IO Bool
windowShouldClose      = GLFW.windowShouldClose
closeGLFW              :: Window -> IO ()
closeGLFW window       = GLFW.setWindowShouldClose window True

-- boolean synonyms
keyPressed             :: GLFW.KeyState -> Bool
keyPressed ks          = ks == GLFW.KeyState'Pressed
keyEscape              :: GLFW.Key -> Bool
keyEscape k            = k == GLFW.Key'Escape



-- used to print GL errors
getGLErrors :: IO ()
getGLErrors = do
  err <- GL.get GL.errors
  mapM_ print err

-- this will use GLFW to create a window and destroy it when closed
withWindow :: Bool -> Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow True ww wh title f = GLFW.terminate
withWindow False ww wh title f = do
  GLFW.setErrorCallback $ Just glfwErrorCallback
  True <- GLFW.init
  Just window <- GLFW.createWindow ww wh title Nothing Nothing
  GLFW.makeContextCurrent $ Just window
  initWindow ww wh
  f window
  GLFW.setErrorCallback $ Just glfwErrorCallback
  GLFW.destroyWindow window
  GLFW.terminate
  where
    glfwErrorCallback e s = putStrLn $ show e ++ " " ++ show s

-- initializes the GL window before the game monad
initWindow :: Int -> Int -> IO ()
initWindow width height = do
  let pos    = GL.Position 0 0
      size   = GL.Size (fromIntegral width) (fromIntegral height)
      h      = fromIntegral height / fromIntegral width :: Double
  do
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GLU.perspective 45 (1/h) (0.1) 500
    GL.matrixMode GL.$= GL.Modelview 0
    GL.loadIdentity
