module Paracletus.Data
  ( ParacExcept(..), ParacResult(..)
  , GraphicsLayer(..)
  ) where
-- supported graphics layers
data GraphicsLayer = Vulkan | OpenGL | OpenGLES deriving (Show)
data ParacExcept = ParacExcept
       { code ∷ Maybe ParacResult
       , msg  ∷ String
       } deriving (Show)
data ParacResult = ParacSuccess | ParacError | GLFWSuccess | GLFWError deriving (Show)
