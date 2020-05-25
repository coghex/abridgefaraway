{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Device where
-- the first suitable device is chosen
import Prelude()
import UPrelude
import Control.Monad
import Data.Bits
import Data.List ((\\))
import qualified Data.Map as Map
import Foreign.Ptr (plusPtr)
import Foreign.C.String (peekCString)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Anamnesis
import Anamnesis.Foreign
import Artos.Log
import Paracletus.Data
import Paracletus.Util
import Paracletus.Vulkan
import Paracletus.Vulkan.Foreign
data DevQueues = DevQueues { graphicsQueue  ∷ VkQueue
                           , presentQueue   ∷ VkQueue
                           , qFamIndices    ∷ Ptr Word32
                           , graphicsFamIdx ∷ Word32
                           , presentFamIdx  ∷ Word32
                           } deriving (Eq, Show)
data SwapchainSupportDetails = SwapchainSupportDetails
  { capabilities ∷ VkSurfaceCapabilitiesKHR
  , formats      ∷ [VkSurfaceFormatKHR]
  , presentModes ∷ [VkPresentModeKHR]
  } deriving (Eq, Show)
selectGraphicsFamily ∷ [(Word32, VkQueueFamilyProperties)] → Anamnesis r e s (Word32, VkQueueFamilyProperties)
selectGraphicsFamily [] = logExcept VulkanError "no graphics family found"
selectGraphicsFamily (x@(_,qfp):xs) = if getField @"queueCount" qfp > 0 ∧ getField @"queueFlags" qfp ⌃ VK_QUEUE_GRAPHICS_BIT ≠ VK_ZERO_FLAGS then pure x else selectGraphicsFamily xs
selectPresentationFamily ∷ VkPhysicalDevice → VkSurfaceKHR → [(Word32, VkQueueFamilyProperties)] → Anamnesis r e s (Word32, VkQueueFamilyProperties)
selectPresentationFamily _ _ [] = logExcept VulkanError "no presentation family found"
selectPresentationFamily dev surf (x@(i,qfp):xs)
  | getField @"queueCount" qfp <= 0 = selectGraphicsFamily xs
  | otherwise = do
    supported ← allocaPeek $ runVk . vkGetPhysicalDeviceSurfaceSupportKHR dev i surf
    if (supported ≡ VK_TRUE) then pure x else selectPresentationFamily dev surf xs
pickPhysicalDevice ∷ VkInstance → Maybe VkSurfaceKHR → Anamnesis r e s (Maybe SwapchainSupportDetails, VkPhysicalDevice)
pickPhysicalDevice vkInstance mVkSurf = do
  devs ← asListVk $ \x → runVk . vkEnumeratePhysicalDevices vkInstance x
  when (null devs) $ logExcept VulkanError "zero device count"
  logInfo $ "found " ⧺ show (length devs) ⧺ " devices"
  selectFirstSuitable devs
  where selectFirstSuitable [] = logExcept VulkanError "no suitable devices..."
        selectFirstSuitable (x:xs) = do
          (mscsd, indeed) ← isDeviceSuitable mVkSurf x
          if indeed then pure (mscsd, x) else selectFirstSuitable xs
isDeviceSuitable ∷ Maybe VkSurfaceKHR → VkPhysicalDevice → Anamnesis r e s (Maybe SwapchainSupportDetails, Bool)
isDeviceSuitable mVkSurf pdev = do
  extsGood ← checkDeviceExtensionSupport pdev [VK_KHR_SWAPCHAIN_EXTENSION_NAME]
  (mscsd, surfGood) ← case mVkSurf of
    Nothing → pure (Nothing, True)
    Just vkSurf
      | not extsGood → pure (Nothing, True)
      | otherwise → do
      scsd@SwapchainSupportDetails {..} ← querySwapchainSupport pdev vkSurf
      return (Just scsd, not (null formats) ∧ not (null presentModes))
  supportedFeatures ← allocaPeek $ liftIO . vkGetPhysicalDeviceFeatures pdev
  let supportsAnisotropy = getField @"samplerAnisotropy" supportedFeatures ≡ VK_TRUE
  pure (mscsd, extsGood ∧ surfGood ∧ supportsAnisotropy)
checkDeviceExtensionSupport ∷ VkPhysicalDevice → [CString] → Anamnesis r e s Bool
checkDeviceExtensionSupport pdev extensions = do
  reqExts ← liftIO $ mapM peekCString extensions
  availExtC ← asListVk $ \x → runVk . vkEnumerateDeviceExtensionProperties pdev VK_NULL_HANDLE x
  availExts ← forM availExtC . flip withVkPtr $ liftIO . peekCString . (`plusPtr` fieldOffset @"extensionName" @VkExtensionProperties)
  return . null $ reqExts \\ availExts
querySwapchainSupport ∷ VkPhysicalDevice → VkSurfaceKHR → Anamnesis r e s SwapchainSupportDetails
querySwapchainSupport pdev surf = do
  capabilities ← allocaPeekVk $ runVk . vkGetPhysicalDeviceSurfaceCapabilitiesKHR pdev surf
  formats ← asListVk $ \x → runVk . vkGetPhysicalDeviceSurfaceFormatsKHR pdev surf x
  presentModes ← asListVk $ \x → runVk . vkGetPhysicalDeviceSurfacePresentModesKHR pdev surf x
  return SwapchainSupportDetails {..}
getMaxUsableSampleCount ∷ VkPhysicalDevice → Anamnesis r e s VkSampleCountFlagBits
getMaxUsableSampleCount pdev = do
  devProps ← allocaPeek $ \propsPtr → liftIO $ vkGetPhysicalDeviceProperties pdev propsPtr
  let limits = getField @"limits" devProps
      colorSampleCounts = getField @"framebufferColorSampleCounts" limits
      depthSampleCounts = getField @"framebufferDepthSampleCounts" limits
      counts = min colorSampleCounts depthSampleCounts
      splitCounts = filter ((≠ VK_ZERO_FLAGS) . (counts .&.)) [ VK_SAMPLE_COUNT_64_BIT, VK_SAMPLE_COUNT_32_BIT, VK_SAMPLE_COUNT_16_BIT, VK_SAMPLE_COUNT_8_BIT, VK_SAMPLE_COUNT_4_BIT, VK_SAMPLE_COUNT_2_BIT, VK_SAMPLE_COUNT_1_BIT ]
      highestCount = head $ splitCounts ⌦ maskToBits
  return highestCount
getQueueFamilies ∷ VkPhysicalDevice → Anamnesis r e s [(Word32, VkQueueFamilyProperties)]
getQueueFamilies pdev = do
  fams ← asListVk $ \c → liftIO . vkGetPhysicalDeviceQueueFamilyProperties pdev c
  when (null fams) $ logExcept VulkanError "zero queue family count"
  return $ zip [0..] fams
createGraphicsDevice ∷ VkPhysicalDevice → VkSurfaceKHR → Anamnesis r e s (VkDevice, DevQueues)
createGraphicsDevice pdev surf
  | layers ← defaultLayers
  , extensions ← [VK_KHR_SWAPCHAIN_EXTENSION_NAME] = do
  qfams ← getQueueFamilies pdev
  (gFamIdx, _gFam) ← selectGraphicsFamily qfams
  (pFamIdx, _pFam) ← selectPresentationFamily pdev surf qfams
  let qFamIndices = Map.fromList [(gFamIdx, gFamIdx), (pFamIdx, pFamIdx)]
  famIndsPtr ← newArrayRes $ Map.elems qFamIndices
  let qcInfoMap = flip fmap qFamIndices $ \qFamIdx → createVk @VkDeviceQueueCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" zeroBits
        &* set @"queueFamilyIndex" qFamIdx
        &* set @"queueCount" 1
        &* setListRef @"pQueuePriorities" [1.0]
      pdevFeatures = createVk @VkPhysicalDeviceFeatures
        $  set @"samplerAnisotropy" VK_TRUE
      devCreateInfo = createVk @VkDeviceCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" zeroBits
        &* setListRef @"pQueueCreateInfos" (Map.elems qcInfoMap)
        &* set @"queueCreateInfoCount" (fromIntegral $ Map.size qcInfoMap)
        &* set @"enabledLayerCount" (fromIntegral $ length layers)
        &* setStrListRef @"ppEnabledLayerNames" layers
        &* set @"enabledExtensionCount" (fromIntegral $ length extensions)
        &* setListRef @"ppEnabledExtensionNames" extensions
        &* setVkRef @"pEnabledFeatures" pdevFeatures
  dev ← allocResource (\dev → liftIO $ vkDestroyDevice dev VK_NULL) $ withVkPtr devCreateInfo $ \dciPtr → allocaPeek $ runVk . vkCreateDevice pdev dciPtr VK_NULL
  gQueues ← flip Map.traverseWithKey qcInfoMap $ \qFamIdx _ → allocaPeek $ liftIO . vkGetDeviceQueue dev qFamIdx 0
  mdevQueues ← maybe (logExcept VulkanError "some queues lost...") pure
               $  DevQueues
              <$> Map.lookup gFamIdx gQueues
              <*> Map.lookup pFamIdx gQueues
              <*> Just famIndsPtr
              <*> Just gFamIdx
              <*> Just pFamIdx
  return (dev, mdevQueues)
-- helper
--maskToBits ∷ (Bits (x FlagMask), Coercible (x FlagBit) (x FlagMask)) ⇒ x FlagMask → [x FlagBit]
--maskToBits x = go (popCount x) (bit 0)
--  where zero   = zeroBits
--        go 0 _ = []
--        go n i = let b = i ⌃ x
--                     i' = unsafeShiftL i 1
--                 in if b ≡ zero
--                    then go n i'
--                    else coerce b : go (n-1) i'
