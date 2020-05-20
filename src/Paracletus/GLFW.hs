{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
module Paracletus.GLFW where
-- GLFW interfaces with paracletus
import Prelude()
import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Anamnesis
import Artos.Event
import Artos.Except
import Paracletus.Data

initGLFWWindow ∷ GraphicsLayer → Int → Int → String → Queue Event → Anamnesis r e s GLFW.Window
initGLFWWindow Vulkan w h n inputChan = do
  allocResource
    (\() → liftIO GLFW.terminate » logInfo "terminated GLFW")
    (liftIO GLFW.init ⌦ flip unless (throwMsg "failed to init glfw"))
  liftIO GLFW.getVersionString ⌦ mapM_ (logInfo . ("glfw version: " ⧺))
  allocResource (\window → do
    liftIO (GLFW.destroyWindow window)
    logDebug "closed glfw window") $ do
      mw ← liftIO $ GLFW.createWindow w h n Nothing Nothing
      case mw of
        Nothing → throwMsg "failed to init glfw"
        Just window → do
          logDebug "initialized glfw window"
          liftIO $ GLFW.setKeyCallback window $ Just $ keyCallback inputChan
          return window
inputGLFWWindow _ _ _ _ _ = throwMsg "unsupported graphics layer"
