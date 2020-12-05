{-# OPTIONS_GHC -fforce-recomp #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Shader where
-- shader creation, compiler in Paracletus.Vulkan.TH
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Anamnesis
import Anamnesis.Foreign
import Anamnesis.Util
import Paracletus.TH
import Paracletus.Vulkan.Foreign


makeTShader ∷ VkDevice → Anamnesis ε σ (VkPipelineShaderStageCreateInfo, VkPipelineShaderStageCreateInfo)
makeTShader dev = do
  shaderVert ← createVkShaderStageCI dev $(compileGLSL ("dat/shd/text.vert")) VK_SHADER_STAGE_VERTEX_BIT
  shaderFrag ← createVkShaderStageCI dev $(compileGLSL ("dat/shd/text.frag")) VK_SHADER_STAGE_FRAGMENT_BIT
  return (shaderVert, shaderFrag)

makeShader ∷ VkDevice → Anamnesis ε σ (VkPipelineShaderStageCreateInfo, VkPipelineShaderStageCreateInfo)
makeShader dev = do
  shaderVert ← createVkShaderStageCI dev $(compileGLSL ("dat/shd/triangle.vert")) VK_SHADER_STAGE_VERTEX_BIT
  shaderFrag ← createVkShaderStageCI dev $(compileGLSL ("dat/shd/triangle.frag")) VK_SHADER_STAGE_FRAGMENT_BIT
  return (shaderVert, shaderFrag)

createVkShaderStageCI ∷ VkDevice → (CSize, Ptr Word32) → VkShaderStageFlagBits → Anamnesis ε σ VkPipelineShaderStageCreateInfo
createVkShaderStageCI dev shaderCode stageBit = do
  shaderModule ← createVulkanShaderModule dev shaderCode
  return $ createVk @VkPipelineShaderStageCreateInfo
    $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
    &* set @"pNext" VK_NULL
    &* set @"stage" stageBit
    &* set @"module" shaderModule
    &* setStrRef @"pName" "main"

createVulkanShaderModule ∷ VkDevice → (CSize, Ptr Word32) → Anamnesis ε σ VkShaderModule
createVulkanShaderModule dev (codeSize, codePtr) = allocResource
  (\sm → liftIO $ vkDestroyShaderModule dev sm VK_NULL) $ withVkPtr smCreateInfo $ \smciPtr → allocaPeek $ runVk ∘ vkCreateShaderModule dev smciPtr VK_NULL
  where smCreateInfo = createVk @VkShaderModuleCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"codeSize" codeSize
          &* set @"pCode" codePtr
          &* set @"flags" VK_ZERO_FLAGS
