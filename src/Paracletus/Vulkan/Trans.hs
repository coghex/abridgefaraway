{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
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
  } deriving (Show, Generic)
instance PrimBytes TransformationObject

updateTransObj ∷ VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis r e s ()
updateTransObj device extent uniBuf = do
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0 (bSizeOf @TransformationObject undefined) VK_ZERO_FLAGS
  -- rotation is identity for now
  let model = DF4
                (DF4 1 0 0 0)
                (DF4 0 1 0 0)
                (DF4 0 0 1 0)
                (DF4 0 0 0 1)
  poke (castPtr uboPtr) (scalar $ TransformationObject {..})
  liftIO $ vkUnmapMemory device uniBuf
  where view  = lookAt (vec3 0 0 1) (vec3 2 2 2) (vec3 0 0 0)
        proj  = proj' %* clip
        proj' = perspective 0.1 20 (45/360*2*pi) aspectRatio
        clip = DF4
          (DF4 1   0   0   0)
          (DF4 0 (-1)  0   0)
          (DF4 0   0  0.5  0)
          (DF4 0   0  0.5  1)
        width = getField @"width" extent
        height = getField @"height" extent
        aspectRatio = fromIntegral width / fromIntegral height

createTransObjBuffers ∷ VkPhysicalDevice → VkDevice → Int → Anamnesis r e s [(VkDeviceMemory, VkBuffer)]
createTransObjBuffers pdev dev n = replicateM n $ createBuffer pdev dev (bSizeOf @TransformationObject undefined) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

transObjBufferInfo ∷ VkBuffer → Anamnesis r e s VkDescriptorBufferInfo
transObjBufferInfo uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (bSizeOf @TransformationObject undefined)
