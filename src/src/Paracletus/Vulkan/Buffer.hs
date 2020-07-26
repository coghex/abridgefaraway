{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Buffer where
import Prelude()
import UPrelude
import Data.Bits (testBit)
import Foreign.Ptr (castPtr)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame
import Numeric.DataFrame
import Anamnesis
import Anamnesis.Foreign
import Anamnesis.Util
import Artos.Except
import Paracletus.Data
import Paracletus.Vulkan.Command
import Paracletus.Vulkan.Foreign
import Paracletus.Vulkan.Vertex

createBuffer ∷ VkPhysicalDevice → VkDevice → VkDeviceSize → VkBufferUsageFlags → VkMemoryPropertyFlags → Anamnesis ε σ (VkDeviceMemory, VkBuffer)
createBuffer pdev dev bSize bUsage bMemPropFlags = do
  let bufferInfo = createVk @VkBufferCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"size" bSize
        &* set @"usage" bUsage
        &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
        &* set @"queueFamilyIndexCount" 0
        &* set @"pQueueFamilyIndices" VK_NULL
  (buf, freeBufLater) ← allocResource'
    (\vb → liftIO $ vkDestroyBuffer dev vb VK_NULL) $
    withVkPtr bufferInfo $ \biPtr → allocaPeek $ runVk ∘ vkCreateBuffer dev biPtr VK_NULL
  memRequirements ← allocaPeek $ liftIO ∘ vkGetBufferMemoryRequirements dev buf
  memIndex ← findMemoryType pdev (getField @"memoryTypeBits" memRequirements) bMemPropFlags
  let allocInfo = createVk @VkMemoryAllocateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"allocationSize" (getField @"size" memRequirements)
        &* set @"memoryTypeIndex" memIndex
  bufferMemory ← allocResource
    (\vbm → liftIO $ vkFreeMemory dev vbm VK_NULL) $
    withVkPtr allocInfo $ \aiPtr → allocaPeek $ runVk ∘ vkAllocateMemory dev aiPtr VK_NULL
  freeBufLater
  runVk $ vkBindBufferMemory dev buf bufferMemory 0
  return (bufferMemory, buf)

copyBuffer ∷ VkDevice → VkCommandPool → VkQueue → VkBuffer → VkBuffer → VkDeviceSize → Anamnesis ε σ ()
copyBuffer dev commandPool cmdQueue srcBuffer dstBuffer bSize = runCommandsOnce dev commandPool cmdQueue $ \cmdBuf → do
  let copyRegion = createVk @VkBufferCopy
        $  set @"srcOffset" 0
        &* set @"dstOffset" 0
        &* set @"size"      bSize
  withVkPtr copyRegion $ liftIO ∘ vkCmdCopyBuffer cmdBuf srcBuffer dstBuffer 1

findMemoryType ∷ VkPhysicalDevice → Word32 → VkMemoryPropertyFlags → Anamnesis ε σ Word32
findMemoryType pdev typeFilter properties = do
  memProps ← allocaPeek $ liftIO ∘ vkGetPhysicalDeviceMemoryProperties pdev
  let mtCount = getField @"memoryTypeCount" memProps
      memTypes = getVec @"memoryTypes" memProps
      go i | i ≡ mtCount = logExcept VulkanError ExParacletus $ "failed to find suitable memory type"
           | otherwise = if testBit typeFilter (fromIntegral i) ∧ (getField @"propertyFlags" (ixOff (fromIntegral i) memTypes) ⌃ properties) ≡ properties then return i else go (i+1)
  go 0

createVertexBuffer ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → DataFrame Vertex '[XN 3] → Anamnesis ε σ VkBuffer
createVertexBuffer pdev dev cmdPool cmdQueue (XFrame vertices) = do
  let bSize = bSizeOf vertices
  (_, vertexBuf) ← createBuffer pdev dev bSize (VK_BUFFER_USAGE_TRANSFER_DST_BIT ⌄ VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  locally $ do
    (stagingMem, stagingBuf) ← createBuffer pdev dev bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
    stagingDataPtr ← allocaPeek $ runVk ∘ vkMapMemory dev stagingMem 0 bSize VK_ZERO_FLAGS
    poke (castPtr stagingDataPtr) vertices
    liftIO $ vkUnmapMemory dev stagingMem
    copyBuffer dev cmdPool cmdQueue stagingBuf vertexBuf bSize
  return vertexBuf

createIndexBuffer ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → DataFrame Word32 '[XN 3] → Anamnesis ε σ VkBuffer
createIndexBuffer pdev dev cmdPool cmdQueue (XFrame indices) = do
  let bSize = bSizeOf indices
  (_, vertexBuf) ← createBuffer pdev dev bSize (VK_BUFFER_USAGE_TRANSFER_DST_BIT ⌄ VK_BUFFER_USAGE_INDEX_BUFFER_BIT) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  locally $ do
    (stagingMem, stagingBuf) ← createBuffer pdev dev bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
    stagingDataPtr ← allocaPeek $ runVk ∘ vkMapMemory dev stagingMem 0 bSize VK_ZERO_FLAGS
    poke (castPtr stagingDataPtr) indices
    liftIO $ vkUnmapMemory dev stagingMem
    copyBuffer dev cmdPool cmdQueue stagingBuf vertexBuf bSize
  return vertexBuf
