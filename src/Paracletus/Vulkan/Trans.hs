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
import Paracletus.Vulkan.Atlas
import Paracletus.Vulkan.Foreign
import Paracletus.Vulkan.Buffer
import Paracletus.Data

data DynTexTransObject = DynTexTransObject
  { dtexi ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes DynTexTransObject

data DynTransObject = DynTransObject
  { move ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes DynTransObject

data TransformationObject = TransformationObject
  { model ∷ Mat44f
  , view  ∷ Mat44f
  , proj  ∷ Mat44f
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
  poke (castPtr uboPtr) (scalar $ TransformationObject {..})
  liftIO $ vkUnmapMemory device uniBuf
  -- these commands are all backwards
  -- ortho near far w h
  where view = translate3 (vec3 (unT 1 cam) (unT 2 cam) (unT 3 cam))
        unT ∷ Int → (Float,Float,Float) → Float
        unT 1 (x,_,_) = 3.6*x
        unT 2 (_,y,_) = 3.6*y
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

updateTransDyn ∷ Int → [DynData] → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransDyn _    []       _      _      _      = return ()
updateTransDyn 0    _        _      _      _      = return ()
updateTransDyn nDyn (dd:dds) device extent uniBuf = do
  let nDyn'   = (fromIntegral nDyn) - 1
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf (nDyn'*(bSizeOf @DynTransObject undefined)) (bSizeOf @DynTransObject undefined) VK_ZERO_FLAGS
  let move = DF4
                (DF4 2 0 0 0)
                (DF4 0 2 0 0)
                (DF4 0 0 2 0)
                (DF4 x y 0 1)
      (x ,y)  = (realToFrac x', realToFrac y')
      (x',y') = ddPosition dd
  poke (castPtr uboPtr) (scalar $ DynTransObject {..})
  liftIO $ vkUnmapMemory device uniBuf
  updateTransDyn (nDyn - 1) dds device extent uniBuf

updateTransTex ∷ Int → [DynData] → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransTex _    []       _      _      _      = return ()
updateTransTex 0    _        _      _      _      = return ()
updateTransTex nDyn (dd:dds) device extent uniBuf = do
  let nDyn'   = (fromIntegral nDyn) - 1
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf (nDyn'*(bSizeOf @DynTexTransObject undefined)) (bSizeOf @DynTexTransObject undefined) VK_ZERO_FLAGS
  let dtexi = DF4
                 (DF4 1 0 0 0)
                 (DF4 0 1 0 0)
                 (DF4 0 0 1 0)
                 (DF4 (x/16.0) (y/6.0) 0 1)
      (x ,y)  = (fromIntegral x', fromIntegral y')
      (x',y') = ddTIndex dd
  poke (castPtr uboPtr) (scalar $ DynTexTransObject {..})
  liftIO $ vkUnmapMemory device uniBuf
  updateTransTex (nDyn - 1) dds device extent uniBuf

createTransObjBuffers ∷ VkPhysicalDevice → VkDevice → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransObjBuffers pdev dev n = replicateM n $ createBuffer pdev dev (bSizeOf @TransformationObject undefined) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

transObjBufferInfo ∷ VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transObjBufferInfo uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (bSizeOf @TransformationObject undefined)

createTransDynBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransDynBuffers pdev dev n nDyn = replicateM n $ createBuffer pdev dev (nDyn'*(bSizeOf @DynTransObject undefined)) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

transDynBufferInfo ∷ Int → VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transDynBufferInfo nDyn uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (nDyn'*(bSizeOf @DynTransObject undefined))
  where nDyn' = max 1 $ fromIntegral nDyn

createTransTexBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransTexBuffers pdev dev n nDyn = replicateM n $ createBuffer pdev dev (nDyn'*(bSizeOf @DynTexTransObject undefined)) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

transTexBufferInfo ∷ Int → VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transTexBufferInfo nDyn uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (nDyn'*(bSizeOf @DynTexTransObject undefined))
  where nDyn' = max 1 $ fromIntegral nDyn
