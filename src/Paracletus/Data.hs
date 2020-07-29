{-# LANGUAGE Strict #-}
module Paracletus.Data where
data GraphicsLayer = GLUnknown | Vulkan | OpenGL | OpenGLES deriving (Show, Eq)
data ParacResult = ParacSuccess | ParacError | GLFWSuccess | GLFWError | VulkanSuccess | VulkanError deriving (Show, Eq)

-- generic tile translates to verticies
-- for each graphics layer, pos is position
-- ind is the texture atlas index, t is the
-- texture index in 2 dimensions
data GTile = GTile { tPos ∷ (Int, Int)
                   , tInd ∷ (Int, Int)
                   , tT   ∷ Int }
