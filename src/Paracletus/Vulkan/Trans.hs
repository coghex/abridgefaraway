{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Trans where
import Prelude()
import UPrelude
import Control.Monad (replicateM)
import Foreign.Ptr (castPtr)
import GHC.Generics (Generic)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Numeric.DataFrame
import Anamnesis
import Anamnesis.Foreign
import Paracletus.Vulkan.Foreign
import Paracletus.Vulkan.Buffer

data TransformationObject = TransformationObject
  { model ∷ Mat44f
  , view  ∷ Mat44f
  , proj  ∷ Mat44f
  , texi  ∷ Vec3i
  } deriving (Show, Generic)
instance PrimBytes TransformationObject

updateTransObj ∷ (Float,Float,Float) → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransObj cam device extent uniBuf = do
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0 (bSizeOf @TransformationObject undefined) VK_ZERO_FLAGS
  let model = DF4
                (DF4 32 0 0 0)
                (DF4 0 32 0 0)
                (DF4 0 0 32 0)
                (DF4 0 0 0  1)
      texi = DF3 1 1 1
  poke (castPtr uboPtr) (scalar $ TransformationObject {..})
  liftIO $ vkUnmapMemory device uniBuf
  -- these commands are all backwards
  -- ortho near far w h
  where view = translate3 (vec3 (unT 1 cam) (unT 2 cam) (unT 3 cam))
        unT ∷ Int → (Float,Float,Float) → Float
        unT 1 (x,_,_) = x
        unT 2 (_,y,_) = y
        unT 3 (_,_,z) = z
        unT _ _ = 0.0
        proj  = proj' %* clip
        proj' = orthogonal (0.1) (500) (fromIntegral width) (fromIntegral height)
        clip = DF4
          (DF4 1   0   0   0)
          (DF4 0 (-1)  0   0)
          (DF4 0   0  0.5  0)
          (DF4 0   0  0.5  1)
        width = getField @"width" extent
        height = getField @"height" extent
        --aspectRatio = fromIntegral width / fromIntegral height

createTransObjBuffers ∷ VkPhysicalDevice → VkDevice → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransObjBuffers pdev dev n = replicateM n $ createBuffer pdev dev (bSizeOf @TransformationObject undefined) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

transObjBufferInfo ∷ VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transObjBufferInfo uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (bSizeOf @TransformationObject undefined)
