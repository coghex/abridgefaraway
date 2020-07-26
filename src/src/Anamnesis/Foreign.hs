{-# language KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
module Anamnesis.Foreign where
import Prelude()
import UPrelude
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import Foreign.Ptr
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable
import qualified GHC.Base as GHC
import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Anamnesis
import Artos.Except
-- modified pointer functions
-- from various libraries
peek ∷ Storable α ⇒ Ptr α → Anamnesis ε σ α
peek = liftIO ∘ Storable.peek
poke ∷ Storable α ⇒ Ptr α → α → Anamnesis ε σ ()
poke p v = liftIO $ Storable.poke p v
alloca ∷ Storable α ⇒ (Ptr α → Anamnesis' ε β) → Anamnesis ε σ β
alloca = liftIOWith Foreign.alloca
allocaPeek ∷ Storable α ⇒ (Ptr α → Anamnesis ε (Either AExcept α) ()) → Anamnesis ε σ α
allocaPeek f = alloca $ \ptr → f ptr ≫ liftIO (Storable.peek ptr)
allocaArray ∷ Storable α ⇒ Int → (Ptr α → Anamnesis' ε β) → Anamnesis ε σ β
allocaArray = liftIOWith ∘ Foreign.allocaArray
peekArray ∷ Storable α ⇒ Int → Ptr α → Anamnesis ε σ [α]
peekArray n = liftIO ∘ Foreign.peekArray n
allocaPeekDF ∷ ∀ α (ns ∷ [Nat]) ε σ. (PrimBytes α, Dimensions ns) ⇒ (Ptr α → Anamnesis ε () ()) → Anamnesis ε σ (DataFrame α ns)
allocaPeekDF pf
  | Dict ← inferKnownBackend @α @ns
  = Anamnesis $ \e s c → do
    mdf ← newPinnedDataFrame
    locVar ← liftIO newEmptyMVar
    withDataFramePtr mdf $ \ptr → unAnamnate (pf ptr) e s (putMVar locVar)
    df ← unsafeFreezeDataFrame mdf
    takeMVar locVar ⌦ c ∘ (df ⚟)
liftIOWith ∷ ((α → IO (Either AExcept β)) → IO (Either AExcept β)) → (α → Anamnesis' ε β) → Anamnesis ε σ β
liftIOWith iof pf = Anamnesis $ \e s c → iof (\a → unAnamnate (pf a) e s pure) ⌦ c
{-# INLINE liftIOWith #-}
-- Res functions release memory after use
mallocArrayRes ∷ Storable α ⇒ Int → Anamnesis ε σ (Ptr α)
mallocArrayRes n = Anamnesis $ \_ _ c → Foreign.allocaArray n (c ∘ Right)
mallocRes ∷ Storable α ⇒ Anamnesis ε σ (Ptr α)
mallocRes = Anamnesis $ \_ _ c → Foreign.alloca (c ∘ Right)
newArrayRes ∷ Storable α ⇒ [α] → Anamnesis ε σ (Ptr α)
newArrayRes xs = Anamnesis $ \_ _ c → Foreign.withArray xs (c ∘ Right)
-- return pointer from dataframes
ptrAtIndex ∷ ∀ α. Storable α ⇒ Ptr α → Int → Ptr α
ptrAtIndex ptr i = ptr `plusPtr` (i * Storable.sizeOf @α undefined)
-- prevents garbage collection
touch ∷ α → IO ()
touch x = GHC.IO $ \s → case GHC.touch# x s of s' → (# s', () #)
{-# INLINE touch #-}
