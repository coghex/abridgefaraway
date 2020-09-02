{-# LANGUAGE Strict #-}
module Paracletus.Data where
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
