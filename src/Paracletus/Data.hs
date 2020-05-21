module Paracletus.Data
  ( ParacExcept(..), ParacResult(..)
  , GraphicsLayer(..)
  ) where
-- supported graphics layers
data GraphicsLayer = Vulkan | OpenGL | OpenGLES deriving (Show, Eq)
data ParacExcept = ParacExcept
       { code ∷ Maybe ParacResult
       , msg  ∷ String
       } deriving (Show, Eq)
data ParacResult = ParacSuccess | ParacError | GLFWSuccess | GLFWError deriving (Show, Eq)
