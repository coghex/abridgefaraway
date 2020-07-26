{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Sync where
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Anamnesis
import Anamnesis.Foreign
import Anamnesis.Util
import Paracletus.Vulkan.Foreign

createSemaphore ∷ VkDevice → Anamnesis ε σ VkSemaphore
createSemaphore dev = allocResource
  (liftIO ∘ flip (vkDestroySemaphore dev) VK_NULL) $ allocaPeek $ \sPtr → withVkPtr
    (createVk
      $  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
    ) $ \ciPtr → runVk $ vkCreateSemaphore dev ciPtr VK_NULL sPtr
createFence ∷ VkDevice → Bool → Anamnesis ε σ VkFence
createFence dev signaled = allocResource
  (liftIO ∘ flip (vkDestroyFence dev) VK_NULL) $ allocaPeek $ \sPtr → withVkPtr
    (createVk
      $  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" (if signaled then VK_FENCE_CREATE_SIGNALED_BIT else VK_ZERO_FLAGS)
    ) $ \ciPtr → runVk $ vkCreateFence dev ciPtr VK_NULL sPtr
