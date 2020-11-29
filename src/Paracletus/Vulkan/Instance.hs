{-# LANGUAGE CPP #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Instance where
import Prelude()
import UPrelude
import Foreign.C.String (peekCString)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Anamnesis
import Anamnesis.Foreign
import Anamnesis.Util
import Paracletus.Vulkan.Foreign
import qualified Paracletus.Oblatum.GLFW as GLFW
-- platform dependent vulkan layers
#ifdef mingw32_HOST_OS
vkLayerValidation ∷ String
vkLayerValidation = "VK_LAYER_KHRONOS_validation"
#else
vkLayerValidation ∷ String
vkLayerValidation = "VK_LAYER_LUNARG_standard_validation"
#endif
-- vulkan gets instantiated from GLFW
createVulkanInstance ∷ String → String → [CString] → [String] → Anamnesis ε σ VkInstance
createVulkanInstance progName engineName extensions layers' = allocResource destroyVulkanInstance $ do
    extStrings ← liftIO $ mapM peekCString extensions
    logDebug $ unlines $ "enabling instance extensions: " : map (" " ⧺) extStrings
    logDebug $ unlines $ "enabling instance layers: " : map (" " ⧺) layers
    withVkPtr iCreateInfo $ \iciPtr → allocaPeek $ runVk ∘ vkCreateInstance iciPtr VK_NULL
  where layers = layers' ⧺ [vkLayerValidation | isDev]
        appInfo = createVk @VkApplicationInfo
          $  set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
          &* set @"pNext" VK_NULL
          &* setStrRef @"pApplicationName" progName
          &* set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
          &* setStrRef @"pEngineName" engineName
          &* set @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
          &* set @"apiVersion" (_VK_MAKE_VERSION 1 0 68)
        iCreateInfo = createVk @VkInstanceCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* setVkRef @"pApplicationInfo" appInfo
          &* set @"enabledLayerCount" (fromIntegral $ length layers)
          &* setStrListRef @"ppEnabledLayerNames" layers
          &* set @"enabledExtensionCount" (fromIntegral $ length extensions)
          &* setListRef @"ppEnabledExtensionNames" extensions

destroyVulkanInstance ∷ VkInstance → Anamnesis ε σ ()
destroyVulkanInstance vkInstance = liftIO (vkDestroyInstance vkInstance VK_NULL) ≫ inDev (logDebug "destroyed vkInstance")
-- vulkan specific glfw function
createGLFWVulkanInstance ∷ String → Anamnesis ε σ VkInstance
createGLFWVulkanInstance progName = do
  glfwReqExts ← liftIO GLFW.getRequiredInstanceExtensions
  createVulkanInstance progName "paracletus" (glfwReqExts) []
