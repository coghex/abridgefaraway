module Paracletus where
-- a graphics layer is chosen,
-- a GLFW instance is begun
import Prelude()
import UPrelude
import Control.Monad.Reader.Class (asks)
import GHC.Stack
import Artos.Except
import Artos.Log
import Anamnesis
import Anamnesis.Data
import Paracletus.Data
-- a generic action is run in a
-- MProg context, returning ()
runParacletus ∷ GraphicsLayer → Anamnesis ret env state ()
runParacletus Vulkan = do
  inputQueue ← asks envEventsChan
  logInfo $ "beginning paracletus..."
  --window ← initGLFWWindow Vulkan 1280 720 "paracletus" inputQueue
  --vulkanInstance ← createGLFWVulkanInstance "paracletus-instance"
  --vulkanSurface ← createSurface vulkanInstance window
  --logDebug $ "createdSurface: " ⧺ show vulkanSurface
runParacletus _ = logExcept ParacError $ "unsupported graphics layer..."
-- *** functions
logExcept ∷ HasCallStack ⇒ ParacResult → String → Anamnesis r e s ()
logExcept ex msg = throwError $ AExcept (Just ex) (msg ⧺ "\n" ⧺ prettyCallStack callStack) "anamnesis"
