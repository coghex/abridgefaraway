{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Paracletus.Vulkan.Data where
-- some auxillary data structs
import Numeric.DataFrame
import Graphics.Vulkan.Core_1_0
import GHC.Generics (Generic)
import Foreign.Storable (Storable, sizeOf, poke, peek, alignment)

-- data required to create a command
-- command buffer from drawState
data CmdBuffData = CmdBuffData
         { cbGQData     ∷ GQData
         , cbPipeline   ∷ VkPipeline
         , cbRenderPass ∷ VkRenderPass
         , cbPipeLayout ∷ VkPipelineLayout
         , cbSwapInfo   ∷ SwapchainInfo
         , cbFramebuffs ∷ [VkFramebuffer]
         , cbDescSets   ∷ VkDescriptorSet }

data SwapchainInfo = SwapchainInfo
         { swapchain     ∷ VkSwapchainKHR
         , swapImgs      ∷ [VkImage]
         , swapImgFormat ∷ VkFormat
         , swapExtent    ∷ VkExtent2D
         } deriving (Eq, Show)

-- the data required to create the texture
-- from the graphicsqueue and command pool
data GQData = GQData
         { pdev    ∷ VkPhysicalDevice
         , dev     ∷ VkDevice
         , cmdPool ∷ VkCommandPool
         , gqueue  ∷ VkQueue }

