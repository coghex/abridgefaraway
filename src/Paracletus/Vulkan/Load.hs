module Paracletus.Vulkan.Load where
-- loading textures from the lua files
-- is spun off as a child thread
import Prelude()
import UPrelude
import Control.Monad.State.Class (gets)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Anamnesis
import Anamnesis.Data
import Paracletus.Vulkan.Desc
import Paracletus.Vulkan.Texture
import Paracletus.Vulkan.Pipeline
import Epiklesis.Lua

-- loads all the textures into layouts of
-- the descriptor sets and pipeline
loadVulkanTextures ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue → Anamnesis ε σ (VkDescriptorSetLayout, VkPipelineLayout, Int, [VkDescriptorImageInfo], VkFormat)
loadVulkanTextures pdev dev cmdPool cmdQueue = do
  settings ← gets sSettings
  -- the engine reserves the first few
  -- textures for default usage.
  -- first texture is a test jpg, second
  -- is the font atlas, third though 11
  -- are a collection of simple text box
  -- textures.
  let tex1Path   = "dat/tex/texture.jpg"
      --texAlph    = "dat/tex/alph.png"
      --texboxPath = "dat/tex/box"
      texAlph = settingFontPath settings
      texboxPath = settingTBPath settings
  boxTexs ← loadNTexs pdev dev cmdPool cmdQueue texboxPath
  (textureView1, mipLevels1) ← createTextureImageView pdev dev cmdPool cmdQueue tex1Path
  (texViewAlph, mipLevelsAlph) ← createTextureImageView pdev dev cmdPool cmdQueue texAlph
  textureSampler1 ← createTextureSampler dev mipLevels1
  texSamplerAlph  ← createTextureSampler dev mipLevelsAlph
  let (btexs, bsamps) = unzip boxTexs
      texViews = [textureView1, texViewAlph] ⧺ btexs
      texSamps = [textureSampler1, texSamplerAlph] ⧺ bsamps
  descriptorTextureInfo ← textureImageInfos texViews texSamps
  depthFormat ← findDepthFormat pdev
  let nimages = length texViews
  descriptorSetLayout ← createDescriptorSetLayout dev nimages
  pipelineLayout ← createPipelineLayout dev descriptorSetLayout
  return (descriptorSetLayout, pipelineLayout, nimages, descriptorTextureInfo, depthFormat)
