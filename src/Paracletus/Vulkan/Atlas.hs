module Paracletus.Vulkan.Atlas where
import Prelude()
import UPrelude
import Graphics.Vulkan.Core_1_0
import Numeric.DataFrame
import Anamnesis
import Paracletus.Vulkan.Texture

-- segment width/height specifies single
-- tex size in pixels, texture width/height
-- the whole image size in segments
data Atlas = Atlas { twidth  ∷ Int
                   , theight ∷ Int
                   , swidth  ∷ Int
                   , sheight ∷ Int
                   , imgview ∷ VkImageView
                   , sampler ∷ VkSampler }

createAtlas ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → (Int, Int) → (Int, Int) → String → Anamnesis ε σ (Atlas)
createAtlas pdev dev commandPool queue (tw, th) (sw, sh) texPath = do
    (textureView, mipLevels) ← createTextureImageView pdev dev commandPool queue texPath
    textureSampler ← createTextureSampler dev mipLevels
    return Atlas { twidth  = tw
                 , theight = th
                 , swidth  = sw
                 , sheight = sh
                 , imgview = textureView
                 , sampler = textureSampler }

indexAtlas ∷ Int → Int → Int → Int → Vec3f → Vec3f
indexAtlas i j w h (Vec3 x y n) = (vec3 x' y' n)
  where x' = (i' + x) / w'
        y' = (j' + y) / h'
        i' = fromIntegral i
        j' = fromIntegral j
        w' = fromIntegral w
        h' = fromIntegral h
