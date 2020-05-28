{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
module Paracletus.Vulkan.Foreign where
-- vulkan specific pointer functions
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Foreign.Marshal.Array as Foreign
import Foreign.Storable (Storable)
import Foreign.Ptr
import qualified Foreign.Storable as Storable
import GHC.Stack
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Artos.Except
import Anamnesis
import Anamnesis.Data
import Anamnesis.Foreign

-- runs io vulkan command,
-- throwing unique exception
runVk ∷ HasCallStack ⇒ IO VkResult → Anamnesis ε σ ()
runVk action = do
  r ← liftIO action
  let ret = AExcept (Just r) ExParacletus $ "vulkan command returned error: " ⧺ (show r) ⧺ "\n\n" ⧺ prettyCallStack callStack
  state $ \s → ((), s { status = ret })
  when (r < VK_SUCCESS) $ throwError ret
{-# INLINE runVk #-}
withVkPtr ∷ VulkanMarshal α ⇒ α → (Ptr α → Anamnesis' ε β) → Anamnesis ε σ β
withVkPtr x = liftIOWith (withPtr x)
{-# INLINE withVkPtr #-}
allocaPeekVk ∷ VulkanMarshal α ⇒ (Ptr α → Anamnesis ε () ()) → Anamnesis ε σ α
allocaPeekVk pf = Anamnesis $ \e s c → do
  locVar ← liftIO newEmptyMVar
  a ← newVkData (\ptr → unAnamnate (pf ptr) e s (putMVar locVar))
  takeMVar locVar ⌦ c ∘ (a ⚟)
asListVk ∷ Storable γ ⇒ (Ptr Word32 → Ptr γ → Anamnesis ε (Either AExcept [γ]) ()) → Anamnesis ε σ [γ]
asListVk action = alloca $ \counterPtr → do
  action counterPtr VK_NULL_HANDLE
  counter ← liftIO $ fromIntegral ⊚ Storable.peek counterPtr
  if counter ≤ 0 then pure [] else allocaArray counter $ \valPtr → do
    action counterPtr valPtr
    liftIO $ Foreign.peekArray counter valPtr
withVkArrayLen ∷ (Storable α, VulkanMarshal α) ⇒ [α] → (Word32 → Ptr α → IO β) → IO β
withVkArrayLen xs pf = do
  ret ← Foreign.withArrayLen xs (pf ∘ fromIntegral)
  touch xs
  return ret
{-# INLINE withVkArrayLen #-}
withArrayLen ∷ (Storable α, VulkanMarshal α) ⇒ [α] → (Word32 → Ptr α → IO β) → IO β
withArrayLen xs pf = do
  ret ← Foreign.withArrayLen xs (pf ∘ fromIntegral)
  touch xs
  return ret
{-# INLINE withArrayLen #-}
