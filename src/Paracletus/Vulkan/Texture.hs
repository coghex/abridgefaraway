{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Texture where
import Prelude()
import UPrelude
import Codec.Picture
import Control.Monad
import Data.List (sort)
import qualified Data.Vector.Storable as Vec
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (castPtr)
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Anamnesis
import Anamnesis.Foreign
import Anamnesis.Util
import Artos.Except
import Paracletus.Data
import Paracletus.Oblatum.Font
import Paracletus.Vulkan.Buffer
import Paracletus.Vulkan.Command
import Paracletus.Vulkan.Foreign

-- this will load all textures in
-- the specified file directory
loadNTexs ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → FilePath → Anamnesis ε σ ([(VkImageView, VkSampler)])
loadNTexs pdev dev cmdPool cmdQueue path = do
  imgPaths ← liftIO $ getDirectoryContents path
  loadNTex pdev dev cmdPool cmdQueue $ map (combine path) $ sort $ filter filterOutPathJunk imgPaths
  where filterOutPathJunk ∷ FilePath → Bool
        filterOutPathJunk "."  = False
        filterOutPathJunk ".." = False
        filterOutPathJunk _    = True

-- recursively load all textures
-- in the given list
loadNTex ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → [FilePath] → Anamnesis ε σ ([(VkImageView, VkSampler)])
loadNTex _ _ _ _ [] = return []
loadNTex pdev dev cmdPool cmdQueue (path:paths) = do
  (imgView, mipLevels) ← createTextureImageView pdev dev cmdPool cmdQueue path
  sampler ← createTextureSampler dev mipLevels
  texs ← loadNTex pdev dev cmdPool cmdQueue paths
  return $ (imgView, sampler) : texs

createTextureImageViews ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → [FilePath] → Anamnesis ε σ ([(VkImageView, Word32)])
createTextureImageViews _    _   _       _        []           = return []
createTextureImageViews pdev dev cmdPool cmdQueue (path:paths) = do
  lt ← (createTextureImageViews pdev dev cmdPool cmdQueue paths)
  nt ← (createTextureImageView pdev dev cmdPool cmdQueue path)
  return $ lt ⧺ [nt]

-- generates vulkan images for each printable
-- ascii character
createFontImageViews ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → FilePath → Int → Anamnesis ε σ ([(VkImageView, Word32)])
createFontImageViews pdev dev cmdPool cmdQueue fp px = mapM (createFontImageView pdev dev cmdPool cmdQueue fp px) ['!'..'~']

-- loads a specified freetype font, generates
-- bitmaps, converts to vulkan image
createFontImageView ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → FilePath → Int → Char → Anamnesis ε σ (VkImageView, Word32)
createFontImageView pdev dev cmdPool cmdQueue fp px char = do
  FontTex w h buff ← liftIO $ loadFTChar fp char px
  let genImg ∷ DynamicImage
      genImg = ImageRGBA8 (generateImage genFunc w h)
      genFunc ∷ Int → Int → PixelRGBA8
      genFunc x y = PixelRGBA8 g g g a
        where g = buff !! (x + (x*y))
              a = if (g < 100) then 0 else 1
  Image { imageWidth, imageHeight, imageData } ← pure $ convertRGBA8 genImg
  let (imageDataForeignPtr, imageDataLen) = Vec.unsafeToForeignPtr0 imageData
      bufSize ∷ VkDeviceSize = fromIntegral imageDataLen
      mipLevels = (floor ∘ logBase (2 ∷ Float) ∘ fromIntegral $ max imageWidth imageHeight) + 1
  (_, image) ← createImage pdev dev
    (fromIntegral imageWidth) (fromIntegral imageHeight) mipLevels VK_SAMPLE_COUNT_1_BIT VK_FORMAT_R8G8B8A8_UNORM VK_IMAGE_TILING_OPTIMAL (VK_IMAGE_USAGE_TRANSFER_SRC_BIT ⌄ VK_IMAGE_USAGE_TRANSFER_DST_BIT ⌄ VK_IMAGE_USAGE_SAMPLED_BIT) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  runCommandsOnce dev cmdPool cmdQueue $ transitionImageLayout image VK_FORMAT_R8G8B8A8_UNORM Undef_TransDst mipLevels
  locally $ do
    (stagingMem, stagingBuf) ← createBuffer pdev dev bufSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )
    stagingDataPtr ← allocaPeek $ runVk ∘ vkMapMemory dev stagingMem 0 bufSize VK_ZERO_FLAGS
    liftIO $ withForeignPtr imageDataForeignPtr $ \imageDataPtr → copyArray (castPtr stagingDataPtr) imageDataPtr imageDataLen
    liftIO $ vkUnmapMemory dev stagingMem
    copyBufferToImage dev cmdPool cmdQueue stagingBuf image (fromIntegral imageWidth) (fromIntegral imageHeight)
  runCommandsOnce dev cmdPool cmdQueue $ generateMipmaps pdev image VK_FORMAT_R8G8B8A8_UNORM (fromIntegral imageWidth) (fromIntegral imageHeight) mipLevels
  imageView ← createImageView dev image VK_FORMAT_R8G8B8A8_UNORM VK_IMAGE_ASPECT_COLOR_BIT mipLevels
  return (imageView, mipLevels)

createTextureImageView ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → FilePath → Anamnesis ε σ (VkImageView, Word32)
createTextureImageView pdev dev cmdPool cmdQueue path = do
  Image { imageWidth, imageHeight, imageData } ← liftIO (readImage path) ⌦ \case
    Left err → logExcept err ExParacletus "cannot create texture image view"
    Right dynImg → pure $ convertRGBA8 dynImg
  let (imageDataForeignPtr, imageDataLen) = Vec.unsafeToForeignPtr0 imageData
      bufSize ∷ VkDeviceSize = fromIntegral imageDataLen
      mipLevels = (floor ∘ logBase (2 ∷ Float) ∘ fromIntegral $ max imageWidth imageHeight) + 1
  (_, image) ← createImage pdev dev
    (fromIntegral imageWidth) (fromIntegral imageHeight) mipLevels VK_SAMPLE_COUNT_1_BIT VK_FORMAT_R8G8B8A8_UNORM VK_IMAGE_TILING_OPTIMAL (VK_IMAGE_USAGE_TRANSFER_SRC_BIT ⌄ VK_IMAGE_USAGE_TRANSFER_DST_BIT ⌄ VK_IMAGE_USAGE_SAMPLED_BIT) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  runCommandsOnce dev cmdPool cmdQueue $ transitionImageLayout image VK_FORMAT_R8G8B8A8_UNORM Undef_TransDst mipLevels
  locally $ do
    (stagingMem, stagingBuf) ← createBuffer pdev dev bufSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )
    stagingDataPtr ← allocaPeek $ runVk ∘ vkMapMemory dev stagingMem 0 bufSize VK_ZERO_FLAGS
    liftIO $ withForeignPtr imageDataForeignPtr $ \imageDataPtr → copyArray (castPtr stagingDataPtr) imageDataPtr imageDataLen
    liftIO $ vkUnmapMemory dev stagingMem
    copyBufferToImage dev cmdPool cmdQueue stagingBuf image (fromIntegral imageWidth) (fromIntegral imageHeight)
  runCommandsOnce dev cmdPool cmdQueue $ generateMipmaps pdev image VK_FORMAT_R8G8B8A8_UNORM (fromIntegral imageWidth) (fromIntegral imageHeight) mipLevels
  imageView ← createImageView dev image VK_FORMAT_R8G8B8A8_UNORM VK_IMAGE_ASPECT_COLOR_BIT mipLevels
  return (imageView, mipLevels)

generateMipmaps ∷ VkPhysicalDevice → VkImage → VkFormat → Word32 → Word32 → Word32 → VkCommandBuffer → Anamnesis ε σ ()
generateMipmaps pdev image format width height mipLevels cmdBuf = do
  formatProps ← allocaPeek $ \propsPtr → liftIO $ vkGetPhysicalDeviceFormatProperties pdev format propsPtr
  let supported = (getField @"optimalTilingFeatures" formatProps) ⌃ VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
   in when (supported ≡ VK_ZERO_FLAGS) $ logExcept VulkanError ExParacletus "texture image format does not support linear blitting"
  mapM_ createLvl
    (zip3
      [1 .. mipLevels - 1]
      (iterate nextLen (fromIntegral width))
      (iterate nextLen (fromIntegral height)))
  let barrier = barrierStruct (mipLevels - 1) VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL VK_ACCESS_TRANSFER_WRITE_BIT VK_ACCESS_SHADER_READ_BIT
   in withVkPtr barrier $ \barrPtr → liftIO $ vkCmdPipelineBarrier cmdBuf VK_PIPELINE_STAGE_TRANSFER_BIT VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT VK_ZERO_FLAGS 0 VK_NULL 0 VK_NULL 1 barrPtr
  where
  nextLen l = if l > 1 then l `div` 2 else 1
  barrierStruct mipLevel oldLayout newLayout srcAccessMask dstAccessMask = createVk @VkImageMemoryBarrier
    $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
    &* set @"pNext" VK_NULL
    &* set @"oldLayout" oldLayout
    &* set @"newLayout" newLayout
    &* set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
    &* set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
    &* set @"image" image
    &* setVk @"subresourceRange"
         (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
         &* set @"baseMipLevel" mipLevel
         &* set @"levelCount" 1
         &* set @"baseArrayLayer" 0
         &* set @"layerCount" 1 )
    &* set @"srcAccessMask" srcAccessMask
    &* set @"dstAccessMask" dstAccessMask
  blitStruct mipLevel srcWidth srcHeight = createVk @VkImageBlit
    $  setAt @"srcOffsets" @0 (createVk
         $  set @"x" 0
         &* set @"y" 0
         &* set @"z" 0 )
    &* setAt @"srcOffsets" @1 (createVk
         $  set @"x" srcWidth
         &* set @"y" srcHeight
         &* set @"z" 1 )
    &* setAt @"dstOffsets" @0 (createVk
         $  set @"x" 0
         &* set @"y" 0
         &* set @"z" 0 )
    &* setAt @"dstOffsets" @1 (createVk
         $  set @"x" (nextLen srcWidth)
         &* set @"y" (nextLen srcHeight)
         &* set @"z" 1 )
    &* setVk @"srcSubresource"
         (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
         &* set @"mipLevel" (mipLevel - 1)
         &* set @"baseArrayLayer" 0
         &* set @"layerCount" 1 )
    &* setVk @"dstSubresource"
         (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
         &* set @"mipLevel" mipLevel
         &* set @"baseArrayLayer" 0
         &* set @"layerCount" 1 )
  createLvl (mipLevel, srcWidth, srcHeight) = do
    let barrier = barrierStruct (mipLevel - 1) VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL VK_ACCESS_TRANSFER_WRITE_BIT VK_ACCESS_TRANSFER_READ_BIT
     in withVkPtr barrier $ \barrPtr → liftIO $ vkCmdPipelineBarrier cmdBuf VK_PIPELINE_STAGE_TRANSFER_BIT VK_PIPELINE_STAGE_TRANSFER_BIT VK_ZERO_FLAGS 0 VK_NULL 0 VK_NULL 1 barrPtr
    withVkPtr (blitStruct mipLevel srcWidth srcHeight) $ \blitPtr → liftIO $ vkCmdBlitImage cmdBuf image VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL image VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL 1 blitPtr VK_FILTER_LINEAR
    let barrier = barrierStruct (mipLevel - 1) VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL VK_ACCESS_TRANSFER_READ_BIT VK_ACCESS_SHADER_READ_BIT
     in withVkPtr barrier $ \barrPtr → liftIO $ vkCmdPipelineBarrier cmdBuf VK_PIPELINE_STAGE_TRANSFER_BIT VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT VK_ZERO_FLAGS 0 VK_NULL 0 VK_NULL 1 barrPtr

createTextureSamplers ∷ VkDevice → [Word32] → Anamnesis e s [VkSampler]
createTextureSamplers _   []                   = return $ []
createTextureSamplers dev (mipLevel:mipLevels) = do
  lt ← createTextureSamplers dev mipLevels
  nt ← createTextureSampler  dev mipLevel
  return $ lt ⧺ [nt]

createTextureSampler ∷ VkDevice → Word32 → Anamnesis e s VkSampler
createTextureSampler dev mipLevels = do
  let samplerCreateInfo = createVk @VkSamplerCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
        &* set @"pNext" VK_NULL_HANDLE
        &* set @"magFilter" VK_FILTER_LINEAR
        &* set @"minFilter" VK_FILTER_LINEAR
        &* set @"addressModeU" VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        &* set @"addressModeV" VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        &* set @"addressModeW" VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        &* set @"anisotropyEnable" VK_TRUE
        &* set @"maxAnisotropy" 16
        &* set @"borderColor" VK_BORDER_COLOR_INT_OPAQUE_BLACK
        &* set @"unnormalizedCoordinates" VK_FALSE
        &* set @"compareEnable" VK_FALSE
        &* set @"compareOp" VK_COMPARE_OP_ALWAYS
        &* set @"mipmapMode" VK_SAMPLER_MIPMAP_MODE_LINEAR
        &* set @"mipLodBias" 0
        &* set @"minLod" 0
        &* set @"maxLod" (fromIntegral mipLevels)
  allocResource (liftIO ∘ flip (vkDestroySampler dev) VK_NULL) $ withVkPtr samplerCreateInfo $ \sciPtr → allocaPeek $ runVk ∘ vkCreateSampler dev sciPtr VK_NULL

textureImageInfos ∷ [VkImageView] → [VkSampler] → Anamnesis ε σ [VkDescriptorImageInfo]
textureImageInfos [] _          = return []
textureImageInfos _  []         = return []
textureImageInfos (v:vs) (s:ss) = do
  t1 ← textureImageInfo v s
  t2 ← textureImageInfos vs ss
  return $ t1 ⧺ t2

textureImageInfo ∷ VkImageView → VkSampler → Anamnesis ε σ [VkDescriptorImageInfo]
textureImageInfo view sampler = return $
  [ createVk @VkDescriptorImageInfo
      $  set @"imageLayout" VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
      &* set @"imageView" view
      &* set @"sampler" sampler ]

createImageView ∷ VkDevice → VkImage → VkFormat → VkImageAspectFlags → Word32 → Anamnesis ε σ VkImageView
createImageView dev image format aspectFlags mipLevels = allocResource (liftIO ∘ flip (vkDestroyImageView dev) VK_NULL) $ withVkPtr imgvCreateInfo $ \imgvciPtr → allocaPeek $ runVk ∘ vkCreateImageView dev imgvciPtr VK_NULL
  where cmapping = createVk
          $  set @"r" VK_COMPONENT_SWIZZLE_IDENTITY
          &* set @"g" VK_COMPONENT_SWIZZLE_IDENTITY
          &* set @"b" VK_COMPONENT_SWIZZLE_IDENTITY
          &* set @"a" VK_COMPONENT_SWIZZLE_IDENTITY
        srrange = createVk
          $  set @"aspectMask" aspectFlags
          &* set @"baseMipLevel" 0
          &* set @"levelCount" mipLevels
          &* set @"baseArrayLayer" 0
          &* set @"layerCount" 1
        imgvCreateInfo = createVk @VkImageViewCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
          &* set @"pNext" VK_NULL_HANDLE
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"image" image
          &* set @"viewType" VK_IMAGE_VIEW_TYPE_2D
          &* set @"format" format
          &* set @"components" cmapping
          &* set @"subresourceRange" srrange

data ImageLayoutTransition = Undef_TransDst | TransDst_ShaderRO | Undef_DepthStencilAtt | Undef_ColorAtt

data TransitionDependent = TransitionDependent
  { oldLayout     ∷ VkImageLayout
  , newLayout     ∷ VkImageLayout
  , srcAccessMask ∷ VkAccessFlags
  , dstAccessMask ∷ VkAccessFlags
  , srcStageMask  ∷ VkPipelineStageFlags
  , dstStageMask  ∷ VkPipelineStageFlags }

dependents ∷ ImageLayoutTransition → TransitionDependent
dependents Undef_TransDst = TransitionDependent
  { oldLayout     = VK_IMAGE_LAYOUT_UNDEFINED
  , newLayout     = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , srcAccessMask = VK_ZERO_FLAGS
  , dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT
  , srcStageMask  = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , dstStageMask  = VK_PIPELINE_STAGE_TRANSFER_BIT }
dependents TransDst_ShaderRO = TransitionDependent
  { oldLayout     = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , newLayout     = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT
  , dstAccessMask = VK_ACCESS_SHADER_READ_BIT
  , srcStageMask  = VK_PIPELINE_STAGE_TRANSFER_BIT
  , dstStageMask  = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT }
dependents Undef_DepthStencilAtt = TransitionDependent
  { oldLayout     = VK_IMAGE_LAYOUT_UNDEFINED
  , newLayout     = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , srcAccessMask = VK_ZERO_FLAGS
  , dstAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT ⌄ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
  , srcStageMask  = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , dstStageMask  = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT }
dependents Undef_ColorAtt = TransitionDependent
  { oldLayout     = VK_IMAGE_LAYOUT_UNDEFINED
  , newLayout     = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , srcAccessMask = VK_ZERO_FLAGS
  , dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT ⌄ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
  , srcStageMask  = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , dstStageMask  = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT }

transitionImageLayout ∷ VkImage → VkFormat → ImageLayoutTransition → Word32 → VkCommandBuffer → Anamnesis ε σ ()
transitionImageLayout image format transition mipLevels cmdBuf = withVkPtr barrier $ \barrPtr → liftIO $ vkCmdPipelineBarrier cmdBuf srcStageMask dstStageMask VK_ZERO_FLAGS 0 VK_NULL 0 VK_NULL 1 barrPtr
  where TransitionDependent{..} = dependents transition
        aspectMask = case newLayout of
          VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
            | hasStencilComponent format → VK_IMAGE_ASPECT_DEPTH_BIT ⌄ VK_IMAGE_ASPECT_STENCIL_BIT
            | otherwise → VK_IMAGE_ASPECT_DEPTH_BIT
          _ → VK_IMAGE_ASPECT_COLOR_BIT
        barrier = createVk @VkImageMemoryBarrier
          $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
          &* set @"pNext" VK_NULL
          &* set @"oldLayout" oldLayout
          &* set @"newLayout" newLayout
          &* set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
          &* set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
          &* set @"image" image
          &* setVk @"subresourceRange"
              (  set @"aspectMask" aspectMask
              &* set @"baseMipLevel" 0
              &* set @"levelCount" mipLevels
              &* set @"baseArrayLayer" 0
              &* set @"layerCount" 1 )
          &* set @"srcAccessMask" srcAccessMask
          &* set @"dstAccessMask" dstAccessMask

createImage ∷ VkPhysicalDevice → VkDevice → Word32 → Word32 → Word32 → VkSampleCountFlagBits → VkFormat → VkImageTiling → VkImageUsageFlags → VkMemoryPropertyFlags → Anamnesis ε σ (VkDeviceMemory, VkImage)
createImage pdev dev width height mipLevels samples format tiling usage propFlags = do
  let ici = createVk @VkImageCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"imageType" VK_IMAGE_TYPE_2D
        &* setVk @"extent"
            (  set @"width" width
            &* set @"height" height
            &* set @"depth" 1 )
        &* set @"mipLevels" mipLevels
        &* set @"arrayLayers" 1
        &* set @"format" format
        &* set @"tiling" tiling
        &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
        &* set @"usage" usage
        &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
        &* set @"samples" samples
        &* set @"queueFamilyIndexCount" 0
        &* set @"pQueueFamilyIndices" VK_NULL
  (image, freeImageLater) ← allocResource'
    (\img → liftIO (vkDestroyImage dev img VK_NULL)) $
    withVkPtr ici $ \iciPtr → allocaPeek $ \imgPtr → runVk $ vkCreateImage dev iciPtr VK_NULL imgPtr
  memRequirements ← allocaPeek $ \reqsPtr → liftIO $ vkGetImageMemoryRequirements dev image reqsPtr
  memType ← findMemoryType pdev (getField @"memoryTypeBits" memRequirements) propFlags
  let allocInfo = createVk @VkMemoryAllocateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"allocationSize" (getField @"size" memRequirements)
        &* set @"memoryTypeIndex" memType
  imageMemory ← allocResource
    (\iMem → liftIO $ vkFreeMemory dev iMem VK_NULL) $
    withVkPtr allocInfo $ \aiPtr → allocaPeek $ runVk ∘ vkAllocateMemory dev aiPtr VK_NULL
  freeImageLater
  runVk $ vkBindImageMemory dev image imageMemory 0
  return (imageMemory, image)

copyBufferToImage ∷ VkDevice → VkCommandPool → VkQueue → VkBuffer → VkImage → Word32 → Word32 → Anamnesis ε σ ()
copyBufferToImage dev cmdPool cmdQueue buffer image width height = runCommandsOnce dev cmdPool cmdQueue $ \cmdBuf → withVkPtr region $ \regPtr → liftIO $ vkCmdCopyBufferToImage cmdBuf buffer image VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL 1 regPtr
  where region = createVk @VkBufferImageCopy
         $  set @"bufferOffset" 0
         &* set @"bufferRowLength" 0
         &* set @"bufferImageHeight" 0
         &* setVk @"imageSubresource"
             (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
             &* set @"mipLevel" 0
             &* set @"baseArrayLayer" 0
             &* set @"layerCount" 1 )
         &* setVk @"imageOffset"
             (  set @"x" 0
             &* set @"y" 0
             &* set @"z" 0 )
         &* setVk @"imageExtent"
             (  set @"width" width
             &* set @"height" height
             &* set @"depth" 1 )

findSupportedFormat ∷ VkPhysicalDevice → [VkFormat] → VkImageTiling → VkFormatFeatureFlags → Anamnesis ε σ VkFormat
findSupportedFormat pdev candidates tiling features = do
  goodCands ← flip filterM candidates $ \format → do
    props ← allocaPeek $ \propsPtr → liftIO $ vkGetPhysicalDeviceFormatProperties pdev format propsPtr
    return $ case tiling of
      VK_IMAGE_TILING_LINEAR → getField @"linearTilingFeatures" props ⌃ features ≡ features
      VK_IMAGE_TILING_OPTIMAL → getField @"optimalTilingFeatures" props ⌃ features ≡ features
      _ → False
  case goodCands of
    x:_ → return x
    []  → logExcept VulkanError ExParacletus "failed to find supported format"

findDepthFormat ∷ VkPhysicalDevice → Anamnesis ε σ VkFormat
findDepthFormat pdev = findSupportedFormat pdev [VK_FORMAT_D32_SFLOAT, VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT] VK_IMAGE_TILING_OPTIMAL VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT

hasStencilComponent ∷ VkFormat → Bool
hasStencilComponent format = format `elem` [VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT]

createDepthAttImgView ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → VkExtent2D → VkSampleCountFlagBits → Anamnesis ε σ VkImageView
createDepthAttImgView pdev dev cmdPool queue extent samples = do
  depthFormat ← findDepthFormat pdev
  (_, depthImage) ← createImage pdev dev
    (getField @"width" extent) (getField @"height" extent) 1 samples depthFormat
    VK_IMAGE_TILING_OPTIMAL VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  depthImageView ← createImageView dev depthImage depthFormat VK_IMAGE_ASPECT_DEPTH_BIT 1
  runCommandsOnce dev cmdPool queue $ transitionImageLayout depthImage depthFormat Undef_DepthStencilAtt 1
  return depthImageView

createColorAttImgView ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → VkFormat → VkExtent2D → VkSampleCountFlagBits → Anamnesis ε σ VkImageView
createColorAttImgView pdev dev cmdPool queue format extent samples = do
  (_, colorImage) ← createImage pdev dev
    (getField @"width" extent) (getField @"height" extent) 1 samples format
    VK_IMAGE_TILING_OPTIMAL (VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT ⌄ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  colorImageView ← createImageView dev colorImage format VK_IMAGE_ASPECT_COLOR_BIT 1
  runCommandsOnce dev cmdPool queue $ transitionImageLayout colorImage format Undef_ColorAtt 1
  return colorImageView
