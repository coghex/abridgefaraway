{-# LANGUAGE Strict #-}
module Paracletus.Vulkan.Foreign where
-- functions are adapted from foreign
import Control.Monad (when)
import GHC.Stack
import Graphics.Vulkan.Core_1_0
--import Graphics.Vulkan.Marshal
import Anamnesis
import Anamnesis.Foreign
import Paracletus.Util
-- runs a vulkan command, for side effects, so
-- we can obtain results and put in Anamnesis
runVk ∷ HasCallStack ⇒ IO VkResult → Anamnesis r e s ()
runVk action = do
  res ← liftIO action
  --state $ \s → ((), s { currentStatus = r })
  when (res < VK_SUCCESS) . logExcept res $ "vulkan command returned an error\n"
withVkPtr ∷ VulkanMarshal a ⇒ a → (Ptr a → Anamnesis' e s b) → Anamnesis r e s b
withVkPtr x = liftIOWith (withPtr x)
