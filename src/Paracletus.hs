module Paracletus where
-- a graphics layer is chosen,
-- a GLFW instance is begun
import Prelude()
import UPrelude
import Control.Monad.Reader.Class (asks)
import Artos.Log
import Anamnesis
import Anamnesis.Data
import Paracletus.Data
import Paracletus.Util
import Paracletus.GLFW
import Paracletus.Vulkan
import Paracletus.Vulkan.Device
import Paracletus.Vulkan.Surface
import Paracletus.Vulkan.Shader
-- a generic action is run in a
-- MProg context, returning ()
runParacletus ∷ GraphicsLayer → Anamnesis ret env state ()
runParacletus Vulkan = do
  inputQueue ← asks envEventsChan
  logInfo $ "beginning paracletus..."
  window ← initGLFWWindow Vulkan 1280 720 "paracletus" inputQueue
  vulkanInstance ← createGLFWVulkanInstance "paracletus-instance"
  vulkanSurface ← createSurface vulkanInstance window
  logDebug $ "createdSurface: " ⧺ show vulkanSurface
  -- fork thread for GLFW
  glfwWaitEventsMeanwhile $ do
    logDebug $ "glfw thread begun..."
    (_, pdev) ← pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logDebug $ "selected physical device: " ⧺ show pdev
    (dev, queues) ← createGraphicsDevice pdev vulkanSurface
    logDebug $ "created device: " ⧺ show dev
    logDebug $ "created queues: " ⧺ show queues
    (shaderVert, shaderFrag) ← makeShader dev
    logDebug $ "created vertex shader module: " ⧺ show shaderVert
    logDebug $ "created fragment shader module: " ⧺ show shaderFrag
runParacletus _ = logExcept ParacError $ "unsupported graphics layer..."
