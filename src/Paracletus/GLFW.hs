{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
module Paracletus.GLFW where
-- GLFW interfaces with paracletus
import Prelude()
import UPrelude
import Control.Monad (unless, forever)
import qualified Graphics.UI.GLFW as GLFW
import Anamnesis
import Anamnesis.Data
import Anamnesis.Init
import Anamnesis.Foreign
import Artos.Log
import Artos.Queue
import Artos.Var
import Paracletus.Data
import Paracletus.Util

-- glfw loop remembers to close window
glfwMainLoop ∷ GLFW.Window → Anamnesis' e s LoopControl → Anamnesis r e s Bool
glfwMainLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            status ← locally action
            if status ≡ ContinueLoop then go else return False
          else return True

initGLFWWindow ∷ GraphicsLayer → Int → Int → String → Queue Event → Anamnesis r e s GLFW.Window
initGLFWWindow Vulkan w h n inputChan = do
  allocResource
    (\() → liftIO GLFW.terminate ≫ logInfo "terminated GLFW")
    (liftIO GLFW.init ⌦ flip unless (logExcept GLFWError "failed to init glfw"))
  liftIO GLFW.getVersionString ⌦ mapM_ (logInfo ∘ ("glfw version: " ⧺))
  allocResource (\window → do
    liftIO (GLFW.destroyWindow window)
    logDebug "closed glfw window") $ do
      mw ← liftIO $ GLFW.createWindow w h n Nothing Nothing
      case mw of
        Nothing → logExcept GLFWError "failed to init glfw"
        Just window → do
          logDebug "initialized glfw window"
          liftIO $ GLFW.setKeyCallback window $ Just $ keyCallback inputChan
          return window
initGLFWWindow OpenGL _ _ _ _ = logExcept GLFWError "OpenGL not yet implemented"
initGLFWWindow OpenGLES _ _ _ _ = logExcept GLFWError "OpenGLES not yet implemented"

glfwWaitEventsMeanwhile ∷ Anamnesis' e s () → Anamnesis r e s ()
glfwWaitEventsMeanwhile action = occupyThreadAndFork (liftIO $ forever $ GLFW.waitEventsTimeout 1.0) (action)

keyCallback ∷ Queue Event → GLFW.Window → GLFW.Key → Int → GLFW.KeyState → GLFW.ModifierKeys → IO ()
keyCallback tc win k sc ka mk = atomically $ writeQueue tc $ EventKey win k sc ka mk
