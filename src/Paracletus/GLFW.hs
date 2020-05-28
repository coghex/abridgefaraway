{-# LANGUAGE Strict #-}
module Paracletus.GLFW where
import Prelude()
import UPrelude
import Control.Monad (when, unless, forever)
import Graphics.UI.GLFW (ClientAPI(..), WindowHint(..))
import qualified Graphics.UI.GLFW as GLFW
import Anamnesis
import Anamnesis.Data
import Anamnesis.Util
import Artos.Except
import Artos.Var
import Paracletus.Data

initGLFWWindow ∷ Int → Int → String → TVar Bool → Anamnesis ε σ GLFW.Window
initGLFWWindow w h n windowSizeChanged = do
  allocResource
    (\() → liftIO GLFW.terminate ≫ logInfo "terminated glfw")
    (liftIO GLFW.init ⌦ flip unless
      (logExcept GLFWError ExParacletus "failed to init glfw")
    )
  liftIO GLFW.getVersionString ⌦ mapM_ (logInfo ∘ ("glfw version: " ⧺))
  liftIO GLFW.vulkanSupported ⌦ flip unless
    (logExcept GLFWError ExParacletus "glfw does not support vulkan")
  liftIO ∘ GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  liftIO ∘ GLFW.windowHint $ WindowHint'Resizable True
  allocResource
    ( \window → do
        liftIO (GLFW.destroyWindow window)
        logDebug "closed glfw window"
    ) $ do
    mw ← liftIO $ GLFW.createWindow w h n Nothing Nothing
    case mw of
      Nothing → logExcept GLFWError ExParacletus "failed to init GLFW"
      Just window → do
        logDebug "initialized glfw window"
        liftIO $ GLFW.setWindowSizeCallback window $
          Just (\_ _ _ → atomically $ writeTVar windowSizeChanged True)
        --liftIO $ GLFW.setKeyCallback window $
        --  Just (\tc win k sc ka mk → atomically $ writeQueue tc $ EventKey win k sc ka mk)
        return window

glfwMainLoop ∷ GLFW.Window → Anamnesis' ε LoopControl → Anamnesis ε σ Bool
glfwMainLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            status ← locally action
            if status ≡ ContinueLoop then go else return False
          else return True
-- runs glfw in the main thread
-- waiting for events every second
glfwWaitEventsMeanwhile ∷ Anamnesis' ε () → Anamnesis ε σ ()
glfwWaitEventsMeanwhile action = occupyThreadAndFork (liftIO $ forever $ GLFW.waitEventsTimeout 1.0) action

-- glfw will wait when minimized
-- so as not to steal input
glfwWaitMinimized ∷ GLFW.Window → Anamnesis ε σ ()
glfwWaitMinimized win = liftIO go where
  go = do
    (x,y) ← GLFW.getFramebufferSize win
    GLFW.waitEvents
    when (x ≡ 0 ∧ y ≡ 0) go
