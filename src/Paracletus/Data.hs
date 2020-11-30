{-# LANGUAGE Strict #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Paracletus.Data where
-- some general data structures are defined
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Numeric.DataFrame
import Artos.Var
import Paracletus.Vulkan.Data
import Paracletus.Vulkan.Vertex
import qualified Paracletus.Oblatum.GLFW as GLFW
data GraphicsLayer = GLUnknown | Vulkan | OpenGL | OpenGLES deriving (Show, Eq)
data ParacResult = ParacSuccess | ParacError | GLFWSuccess | GLFWError | VulkanSuccess | VulkanError deriving (Show, Eq)

-- the fundamental verteces
data Verts = Verts (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])

-- a data structure containing
-- the abstract representation
-- of the general vertex layout
-- any structures here will be
-- decoded every frame so keep
-- them as fundamental as possible
data DrawState = DrawState { dsTiles ∷ [GTile] }

-- generic tile translates to verticies
-- for each graphics layer, pos is position
-- on the screen, ind is the texture atlas
-- index, t is the texture index in 2 dimensions
-- size is the size of the atlas
data GTile = GTileCached   { gtData ∷ ([DataFrame Vertex ('[] ∷ [Nat])]) }
           | GTileUncached { tPos   ∷ (Double,Double)
                           , tScale ∷ (Double,Double)
                           , tInd   ∷ (Int,Int)
                           , tSize  ∷ (Int,Int)
                           , tT     ∷ Int
                           , tTile  ∷ Bool
                           , tMoves ∷ Bool }-- deriving (Show, Eq)


-- default gtile provides interface similar
-- to the optional arguments found in C
defaultGTile ∷ GTile
defaultGTile = GTileUncached { tPos   = (0,0)
                             , tScale = (1,1)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tT     = 0
                             , tTile  = False
                             , tMoves = False }

-- data for dynamic object transformations
data DynData = DynData
         { ddPosition ∷ (Float,Float)
         , ddTIndex   ∷ (Int,Int)
         } deriving (Show, Eq)

-- all the data required for a set of textures
data TextureData = TextureData
         { descSetLayout  ∷ VkDescriptorSetLayout
         , pipelineLayout ∷ VkPipelineLayout
         , nimages        ∷ Int
         , descTexInfo    ∷ [VkDescriptorImageInfo]
         , depthFormat    ∷ VkFormat }

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
