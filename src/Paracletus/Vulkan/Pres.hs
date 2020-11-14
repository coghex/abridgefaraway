{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Pres where
import Prelude()
import UPrelude
import Data.Maybe (fromMaybe)
import Data.Semigroup
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Anamnesis
import Anamnesis.Foreign
import Anamnesis.Util
import Artos.Except
import Paracletus.Data
import Paracletus.Vulkan.Data
import Paracletus.Vulkan.Foreign

createSurface ∷ VkInstance → GLFW.Window → Anamnesis ε σ VkSurfaceKHR
createSurface vkInstance window = allocResource
  (\s → liftIO $ vkDestroySurfaceKHR vkInstance s VK_NULL) $ allocaPeek $ runVk ∘ GLFW.createWindowSurface vkInstance window VK_NULL

chooseSwapSurfaceFormat ∷ SwapchainSupportDetails → Anamnesis ε σ VkSurfaceFormatKHR
chooseSwapSurfaceFormat SwapchainSupportDetails {..} = maybe (logExcept VulkanError ExParacletus "No available surface formats!") (pure ∘ argVal ∘ getMin) ∘ getOption $ foldMap (Option ∘ Just ∘ Min ∘ fmtCost) formats
  where argVal (Arg _ b) = b
        bestFmt ∷ VkSurfaceFormatKHR
        bestFmt = createVk @VkSurfaceFormatKHR
          $  set @"format" VK_FORMAT_B8G8R8A8_UNORM
          &* set @"colorSpace" VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
        fmtCost ∷ VkSurfaceFormatKHR → Arg Int VkSurfaceFormatKHR
        fmtCost f = case (getField @"format" f, getField @"colorSpace" f) of
          (VK_FORMAT_UNDEFINED, _) → Arg 0 bestFmt
          (VK_FORMAT_B8G8R8A8_UNORM, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) → Arg 1 f
          (_, _) → Arg 2 f

chooseSwapPresentMode ∷ SwapchainSupportDetails → VkPresentModeKHR
chooseSwapPresentMode SwapchainSupportDetails {..} = argVal ∘ getMin ∘ fromMaybe (Min $ Arg 0 VK_PRESENT_MODE_FIFO_KHR) ∘ getOption $ foldMap (Option ∘ Just ∘ pmCost) presentModes
  where argVal (Arg _ b) = b
        pmCost ∷ VkPresentModeKHR → ArgMin Int VkPresentModeKHR
        pmCost VK_PRESENT_MODE_MAILBOX_KHR = Min $ Arg 0 VK_PRESENT_MODE_MAILBOX_KHR
        pmCost VK_PRESENT_MODE_IMMEDIATE_KHR = Min $ Arg 1 VK_PRESENT_MODE_IMMEDIATE_KHR
        pmCost VK_PRESENT_MODE_FIFO_KHR = Min $ Arg 2 VK_PRESENT_MODE_FIFO_KHR
        pmCost pm = Min $ Arg 3 pm

chooseSwapExtent ∷ SwapchainSupportDetails → VkExtent2D
chooseSwapExtent SwapchainSupportDetails {..} = createVk @VkExtent2D 
  $  set @"width" (max (ew $ getField @"minImageExtent" capabilities)
                 $ min (ew $ getField @"maxImageExtent" capabilities)
                       (ew $ getField @"currentExtent" capabilities))
  &* set @"height" (max (eh $ getField @"minImageExtent" capabilities)
                 $ min (eh $ getField @"maxImageExtent" capabilities)
                       (eh $ getField @"currentExtent" capabilities))
  where ew = getField @"width"
        eh = getField @"height"

createSwapchain ∷ VkDevice → SwapchainSupportDetails → DevQueues → VkSurfaceKHR → Anamnesis ε σ SwapchainInfo
createSwapchain dev scsd queues surf = do
  surfFmt ← chooseSwapSurfaceFormat scsd
  let maxIC = getField @"maxImageCount" $ capabilities scsd
      minIC = getField @"minImageCount" $ capabilities scsd
      imageCount = if maxIC ≤ 0 then minIC + 1 else min maxIC (minIC + 1)
      spMode = chooseSwapPresentMode scsd
      sExtent = chooseSwapExtent scsd
      swCreateInfo = createVk @VkSwapchainCreateInfoKHR
          $  set @"sType" VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
          &* set @"pNext" VK_NULL_HANDLE
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"surface" surf
          &* set @"minImageCount" imageCount
          &* set @"imageFormat" (getField @"format" surfFmt)
          &* set @"imageColorSpace" (getField @"colorSpace" surfFmt)
          &* set @"imageExtent" sExtent
          &* set @"imageArrayLayers" 1
          &* set @"imageUsage" VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
          &*
          ( if graphicsQueue queues ≠ presentQueue queues
            then set @"imageSharingMode" VK_SHARING_MODE_CONCURRENT
              &* set @"queueFamilyIndexCount" 2
              &* set @"pQueueFamilyIndices" (qFamIndices queues)
            else set @"imageSharingMode" VK_SHARING_MODE_EXCLUSIVE
              &* set @"queueFamilyIndexCount" 0
              &* set @"pQueueFamilyIndices" VK_NULL_HANDLE )
          &* set @"preTransform" (getField @"currentTransform" $ capabilities scsd)
          &* set @"compositeAlpha" VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
          &* set @"presentMode" spMode
          &* set @"clipped" VK_TRUE
          &* set @"oldSwapchain" VK_NULL_HANDLE
  logDebug $ "available present modes: " ⧺ show (presentModes scsd)
  logDebug $ "using present mode: " ⧺ show spMode
  swapchain ← allocResource
    (\swapchain → liftIO $ vkDestroySwapchainKHR dev swapchain VK_NULL) $
    withVkPtr swCreateInfo $ \swciPtr → allocaPeek $ runVk ∘ vkCreateSwapchainKHR dev swciPtr VK_NULL
  swapImgs ← asListVk $ \x → runVk ∘ vkGetSwapchainImagesKHR dev swapchain x
  return SwapchainInfo { swapchain     = swapchain
                       , swapImgs      = swapImgs
                       , swapImgFormat = getField @"format" surfFmt
                       , swapExtent    = sExtent }
  
