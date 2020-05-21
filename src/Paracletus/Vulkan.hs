{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan where
-- some glue-like functions for
-- interfacing to the vulkan-api
import Prelude()
import UPrelude
import Foreign.C.String (peekCString)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Anamnesis
import Anamnesis.Foreign
import Artos.Log
import Paracletus.Vulkan.Foreign
createGLFWVulkanInstance ∷ String → Anamnesis r e s VkInstance
createGLFWVulkanInstance progName = do
  glfwReqExts ← liftIO GLFW.getRequiredInstanceExtensions
  createVulkanInstance progName "paracletus" glfwReqExts []
createVulkanInstance ∷ String → String → [CString] → [String] → Anamnesis r e s VkInstance
createVulkanInstance progName engineName extensions layers' = allocResource destroyVulkanInstance $ do
  extStrings ← liftIO $ mapM peekCString extensions
  logDebug $ unlines $ "enabled instance extensions: " : map (" " ⧺) extStrings
  logDebug $ unlines $ "enabled instance layers: " : map (" " ⧺) layers
  withVkPtr iCreateInfo $ \iciPtr → allocaPeek $ runVk . vkCreateInstance iciPtr VK_NULL
  where layers = layers' ⧺ defaultLayers
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
destroyVulkanInstance ∷ VkInstance → Anamnesis r e s ()
destroyVulkanInstance vkInstance = liftIO (vkDestroyInstance vkInstance VK_NULL) >> inDev (logDebug "destroyed vkInstance")
defaultLayers ∷ [String]
defaultLayers = ["VK_LAYER_LUNARG_standard_validation" | isDev]
