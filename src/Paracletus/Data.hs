{-# LANGUAGE Strict #-}
module Paracletus.Data where
-- some general data structures are defined
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Artos.Var
import qualified Paracletus.Oblatum.GLFW as GLFW
data GraphicsLayer = GLUnknown | Vulkan | OpenGL | OpenGLES deriving (Show, Eq)
data ParacResult = ParacSuccess | ParacError | GLFWSuccess | GLFWError | VulkanSuccess | VulkanError deriving (Show, Eq)

-- generic tile translates to verticies
-- for each graphics layer, pos is position
-- on the screen, ind is the texture atlas
-- index, t is the texture index in 2 dimensions
-- size is the size of the atlas
data GTile = GTile { tPos   ∷ (Double,Double)
                   , tScale ∷ (Double,Double)
                   , tInd   ∷ (Int, Int)
                   , tSize  ∷ (Int, Int)
                   , tT     ∷ Int
                   , tMoves ∷ Bool } deriving (Show, Eq)
-- default gtile provides interface similar
-- to the optional arguments found in C
defaultGTile ∷ GTile
defaultGTile = GTile { tPos   = (0,0)
                     , tScale = (1,1)
                     , tInd   = (0,0)
                     , tSize  = (1,1)
                     , tT     = 0
                     , tMoves = False }

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

-- the main data for the vulkan loop
data VulkanLoopData = VulkanLoopData
         { gqdata             ∷ GQData
         , queues             ∷ DevQueues
         , scsd               ∷ SwapchainSupportDetails
         , window             ∷ GLFW.Window
         , vulkanSurface      ∷ VkSurfaceKHR
         , texData            ∷ TextureData
         , msaaSamples        ∷ VkSampleCountFlagBits
         , shaderVert         ∷ VkPipelineShaderStageCreateInfo
         , shaderFrag         ∷ VkPipelineShaderStageCreateInfo
         , imgIndexPtr        ∷ Ptr Word32
         , windowSizeChanged  ∷ TVar Bool
         , frameIndexRef      ∷ TVar Int
         , renderFinishedSems ∷ Ptr VkSemaphore
         , imageAvailableSems ∷ Ptr VkSemaphore
         , inFlightFences     ∷ Ptr VkFence }

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
