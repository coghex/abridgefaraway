{-# LANGUAGE Strict #-}
module Paracletus.Data where
-- some general data structures are defined
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
data GraphicsLayer = GLUnknown | Vulkan | OpenGL | OpenGLES deriving (Show, Eq)
data ParacResult = ParacSuccess | ParacError | GLFWSuccess | GLFWError | VulkanSuccess | VulkanError deriving (Show, Eq)

-- generic tile translates to verticies
-- for each graphics layer, pos is position
-- on the screen, ind is the texture atlas
-- index, t is the texture index in 2 dimensions
-- size is the size of the atlas
data GTile = GTile { tPos   ∷ (Float, Float)
                   , tScale ∷ (Float, Float)
                   , tInd   ∷ (Int, Int)
                   , tSize  ∷ (Int, Int)
                   , tT     ∷ Int }
-- default gtile provides interface similar
-- to the optional arguments found in C
defaultGTile = GTile { tPos   = (0,0)
                     , tScale = (1,1)
                     , tInd   = (0,0)
                     , tSize  = (1,1)
                     , tT     = 0 }

-- all the data required for a set of textures
data TextureData = TextureData
         { descSetLayout  ∷ VkDescriptorSetLayout
         , pipelineLayout ∷ VkPipelineLayout
         , nimages        ∷ Int
         , descTexInfo    ∷ [VkDescriptorImageInfo]
         , depthFormat    ∷ VkFormat }

-- the data required to create the texture
-- from the graphicsqueue and command pool
data GQData = GQData
         { pdev    ∷ VkPhysicalDevice
         , dev     ∷ VkDevice
         , cmdPool ∷ VkCommandPool
         , gqueue  ∷ VkQueue }
