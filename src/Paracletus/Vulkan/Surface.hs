{-# LANGUAGE Strict #-}
module Paracletus.Vulkan.Surface where
-- the surface logic and swapchain are defined
import Control.Monad.Trans
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan
import Graphics.Vulkan.Ext.VK_KHR_surface
import Anamnesis
import Anamnesis.Foreign
import Paracletus.Vulkan.Foreign

createSurface :: VkInstance -> GLFW.Window -> Anamnesis r e s VkSurfaceKHR
createSurface vkInstance window = allocResource (\s -> liftIO $ vkDestroySurfaceKHR vkInstance s VK_NULL) $ allocaPeek $ runVk . GLFW.createWindowSurface vkInstance window VK_NULL
