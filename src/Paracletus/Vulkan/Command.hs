{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Command where
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Anamnesis
import Anamnesis.Foreign
import Paracletus.Vulkan.Foreign
import Paracletus.Vulkan.Device

createCommandPool ∷ VkDevice → DevQueues → Anamnesis r e s VkCommandPool
createCommandPool dev DevQueues{..} =
  allocResource (liftIO ∘ flip (vkDestroyCommandPool dev) VK_NULL) $ allocaPeek $ \pPtr → withVkPtr
    ( createVk
      $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"queueFamilyIndex" graphicsFamIdx
    ) $ \ciPtr → runVk $ vkCreateCommandPool dev ciPtr VK_NULL pPtr
