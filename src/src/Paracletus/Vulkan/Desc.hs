{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Desc where
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Anamnesis
import Anamnesis.Foreign
import Anamnesis.Util
import Paracletus.Vulkan.Foreign

createDescriptorPool ∷ VkDevice → Int → Anamnesis ε σ VkDescriptorPool
createDescriptorPool dev n = allocResource (liftIO ∘ flip (vkDestroyDescriptorPool dev) VK_NULL) $ allocaPeek $ \pPtr → withVkPtr (createVk
    $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
    &* set @"pNext" VK_NULL
    &* set @"flags" VK_ZERO_FLAGS
    &* setListCountAndRef @"poolSizeCount" @"pPoolSizes"
      [ createVk @VkDescriptorPoolSize
        $  set @"type" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        &* set @"descriptorCount" (fromIntegral n)
      , createVk @VkDescriptorPoolSize
        $  set @"type" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        &* set @"descriptorCount" (fromIntegral n) ]
    &* set @"maxSets" (fromIntegral n)
  ) $ \ciPtr → runVk $ vkCreateDescriptorPool dev ciPtr VK_NULL pPtr

createDescriptorSetLayout ∷ VkDevice → Anamnesis ε σ VkDescriptorSetLayout
createDescriptorSetLayout dev = allocResource
  (\dsl → liftIO $ vkDestroyDescriptorSetLayout dev dsl VK_NULL) $
  withVkPtr dslCreateInfo $ \dslciPtr → allocaPeek $ runVk ∘ vkCreateDescriptorSetLayout dev dslciPtr VK_NULL
  where dslCreateInfo = createVk @VkDescriptorSetLayoutCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* setListCountAndRef @"bindingCount" @"pBindings"
              [ createVk @VkDescriptorSetLayoutBinding
                $  set @"binding" 0
                &* set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                &* set @"descriptorCount" 1
                &* set @"stageFlags" VK_SHADER_STAGE_VERTEX_BIT
                &* set @"pImmutableSamplers" VK_NULL
              , createVk @VkDescriptorSetLayoutBinding
                $  set @"binding" 1
                &* set @"descriptorType" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                &* set @"descriptorCount" 1
                &* set @"stageFlags" VK_SHADER_STAGE_FRAGMENT_BIT
                &* set @"pImmutableSamplers" VK_NULL ]

createDescriptorSets ∷ VkDevice → VkDescriptorPool → Int → Ptr VkDescriptorSetLayout → Anamnesis ε σ [VkDescriptorSet]
createDescriptorSets dev descriptorPool n layoutsPtr = allocaArray n $ \dsPtr → withVkPtr dsai $ \dsaiPtr → do
  runVk $ vkAllocateDescriptorSets dev dsaiPtr dsPtr
  peekArray n dsPtr
  where dsai = createVk @VkDescriptorSetAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"descriptorPool" descriptorPool
          &* set @"descriptorSetCount" (fromIntegral n)
          &* set @"pSetLayouts" layoutsPtr

prepareDescriptorSet ∷ VkDevice → VkDescriptorBufferInfo → VkDescriptorImageInfo → VkDescriptorSet → Anamnesis ε σ ()
prepareDescriptorSet dev bufferInfo imageInfo descriptorSet = liftIO $ withVkArrayLen descriptorWrites $ \dwLen dwPtr → liftIO $ vkUpdateDescriptorSets dev dwLen dwPtr 0 VK_NULL
  where descriptorWrites =
          [ createVk @VkWriteDescriptorSet
            $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
            &* set @"pNext" VK_NULL
            &* set @"dstSet" descriptorSet
            &* set @"dstBinding" 0
            &* set @"dstArrayElement" 0
            &* set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
            &* set @"descriptorCount" 1
            &* setVkRef @"pBufferInfo" bufferInfo
            &* set @"pImageInfo" VK_NULL
            &* set @"pTexelBufferView" VK_NULL
          , createVk @VkWriteDescriptorSet
            $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
            &* set @"pNext" VK_NULL
            &* set @"dstSet" descriptorSet
            &* set @"dstBinding" 1
            &* set @"dstArrayElement" 0
            &* set @"descriptorType" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            &* set @"descriptorCount" 1
            &* set @"pBufferInfo" VK_NULL
            &* setVkRef @"pImageInfo" imageInfo
            &* set @"pTexelBufferView" VK_NULL ]
