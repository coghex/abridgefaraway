{-# LANGUAGE Strict #-}
module Paracletus.Vulkan.Foreign where
-- functions are adapted from foreign
import Prelude()
import UPrelude
import Control.Concurrent.MVar
import Control.Monad (when)
import qualified Foreign.Marshal.Array as Foreign
import Foreign.Ptr
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable
import GHC.Stack
import Graphics.Vulkan.Core_1_0
import Anamnesis
import Anamnesis.Foreign
import Artos.Except
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
-- returns data from vulkan-api
allocaPeekVk ∷ VulkanMarshal a ⇒ (Ptr a → Anamnesis () e s ()) → Anamnesis r e s a
allocaPeekVk pf = Anamnesis $ \ref env st c → do
  locVar ← liftIO newEmptyMVar
  a ← newVkData (\ptr → unAnamnate (pf ptr) ref env st (putMVar locVar))
  takeMVar locVar ⌦ c ∘ (a ⚟)
asListVk ∷ Storable x ⇒ (Ptr Word32 → Ptr x → Anamnesis (Either AExcept [x]) e s ()) → Anamnesis r e s [x]
asListVk action = alloca $ \counterPtr → do
  action counterPtr VK_NULL_HANDLE
  counter ← liftIO $ fromIntegral ⊚ Storable.peek counterPtr
  if counter <= 0 then pure [] else allocaArray counter $ \valPtr → do
    action counterPtr valPtr
    liftIO $ Foreign.peekArray counter valPtr
withVkArrayLen ∷ (Storable a, VulkanMarshal a) ⇒ [a] → (Word32 → Ptr a → Anamnesis' e s b) → Anamnesis r e s b
withVkArrayLen xs pf = liftIOWith (withArrayLen xs ∘ curry) (uncurry pf)
withArrayLen ∷ (Storable a, VulkanMarshal a) ⇒ [a] → (Word32 → Ptr a → IO b) → IO b
withArrayLen xs pf = do
  ret ← Foreign.withArrayLen xs (pf ∘ fromIntegral)
  touch xs
  return ret
