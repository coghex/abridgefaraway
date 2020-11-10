module Paracletus.Vulkan.Load where
-- loading textures from the lua files
-- is spun off as a child thread
import Prelude()
import UPrelude
import Control.Monad.State.Class (gets)
import Anamnesis
import Anamnesis.Data
import Paracletus.Data
import Paracletus.Vulkan.Desc
import Paracletus.Vulkan.Texture
import Paracletus.Vulkan.Pipeline

-- loads all the textures into layouts of
-- the descriptor sets and pipeline. an
-- empty string will just load default textures
-- and filepaths added will be ammended to that
loadVulkanTextures ∷ GQData → [FilePath] → Anamnesis ε σ (TextureData)
loadVulkanTextures (GQData pdev dev cmdPool cmdQueue) fps = do
  -- the engine reserves the first few
  -- textures for default usage.
  -- first texture is a test jpg, second
  -- is the font atlas, third though 11
  -- are a collection of simple text box
  -- textures. what follows is dynamic
  -- texture data, first an array of
  -- generic tiles that can be created,
  -- then the list of world textures
  let tex1Path    = "dat/tex/texture.jpg"
      texAlph     = "dat/tex/alph.png"
      texboxPath  = "dat/tex/box"
      texmboxPath = "dat/tex/mbox"
      --texAlph = settingFontPath settings
      --texboxPath = settingTBPath settings
      --texmboxPath = settingMTBPath settings
  boxTexs ← loadNTexs pdev dev cmdPool cmdQueue texboxPath
  mboxTexs ← loadNTexs pdev dev cmdPool cmdQueue texmboxPath
  (textureView1, mipLevels1) ← createTextureImageView pdev dev cmdPool cmdQueue tex1Path
  (texViewAlph, mipLevelsAlph) ← createTextureImageView pdev dev cmdPool cmdQueue texAlph
  modTexViews ← createTextureImageViews pdev dev cmdPool cmdQueue fps
  textureSampler1 ← createTextureSampler dev mipLevels1
  texSamplerAlph  ← createTextureSampler dev mipLevelsAlph
  texSamplersMod  ← createTextureSamplers dev $ snd . unzip $ modTexViews
  let (btexs, bsamps) = unzip boxTexs
      (mbtexs, mbsamps) = unzip mboxTexs
      texViews = [textureView1, texViewAlph] ⧺ btexs ⧺ mbtexs ⧺ (fst (unzip modTexViews))
      texSamps = [textureSampler1, texSamplerAlph] ⧺ bsamps ⧺ mbsamps ⧺ texSamplersMod
  descriptorTextureInfo ← textureImageInfos texViews texSamps
  depthFormat ← findDepthFormat pdev
  let nimages = length texViews
  descriptorSetLayout ← createDescriptorSetLayout dev nimages
  pipelineLayout ← createPipelineLayout dev descriptorSetLayout
  return $ TextureData descriptorSetLayout pipelineLayout nimages descriptorTextureInfo depthFormat
