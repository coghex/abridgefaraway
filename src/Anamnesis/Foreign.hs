{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
module Anamnesis.Foreign where
-- a collection of memory operations
import Prelude()
import UPrelude
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import Foreign.Storable (Storable)
import Foreign.Ptr
import qualified Foreign.Storable as Storable
import qualified GHC.Base as GHC
import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Anamnesis
import Artos.Except
-- control.exception try-bracket modified
try ∷ Anamnesis r e s a → Anamnesis r e s (Either AExcept a)
try a = Anamnesis $ \ret env st c → unAnamnate a ret env st $ c ∘ Right
bracket ∷ Anamnesis r e s a → (a → Anamnesis r e s b) → (a → Anamnesis r e s c) → Anamnesis r e s c
bracket before after thing = do
  a ← before
  er ← try (thing a)
  _ ← after a
  Anamnesis $ const $ const $ const ($ er)
-- run nested continuations locally
locally ∷ Anamnesis' e s a → Anamnesis r e s a
locally p = Anamnesis $ \ref env st c → unAnamnate p ref env st pure ⌦ c
-- prevent garbage collection
touch ∷ a → IO ()
touch x = GHC.IO $ \s → case GHC.touch# x s of s' → (# s', () #)
-- various pointer operations
peek ∷ Storable a ⇒ Ptr a → Anamnesis r e s a
peek = liftIO ∘ Storable.peek
poke ∷ Storable a ⇒ Ptr a → a → Anamnesis r e s ()
poke p v = liftIO $ Storable.poke p v
peekArray ∷ Storable a ⇒ Int → Ptr a → Anamnesis r e s [a]
peekArray n = liftIO ∘ Foreign.peekArray n
alloca ∷ Storable a ⇒ (Ptr a → Anamnesis' e s b) → Anamnesis r e s b
alloca = liftIOWith Foreign.alloca
allocaPeek ∷ Storable a ⇒ (Ptr a → Anamnesis (Either AExcept a) e s ()) → Anamnesis r e s a
allocaPeek f = alloca $ \ptr → f ptr ≫ liftIO (Storable.peek ptr)
allocaArray ∷ Storable a ⇒ Int → (Ptr a → Anamnesis' e s b) → Anamnesis r e s b
allocaArray = liftIOWith . Foreign.allocaArray
mallocRes ∷ Storable a ⇒ Anamnesis r e s (Ptr a)
mallocRes = Anamnesis $ \_ _ _ c → Foreign.alloca (c ∘ Right)
newArrayRes ∷ Storable a ⇒ [a] → Anamnesis r e s (Ptr a)
newArrayRes xs = Anamnesis $ \_ _ _ c → Foreign.withArray xs (c . Right)
mallocArrayRes ∷ Storable a ⇒ Int → Anamnesis r e s (Ptr a)
mallocArrayRes n = Anamnesis $ \_ _ _ c → Foreign.allocaArray n (c ∘ Right)
ptrAtIndex ∷ ∀ a. Storable a ⇒ Ptr a → Int → Ptr a
ptrAtIndex ptr i = ptr `plusPtr` (i * Storable.sizeOf @a undefined)
-- functions to help with pointers
allocResource ∷ (a → Anamnesis' e s ()) → Anamnesis r e s a → Anamnesis r e s a
allocResource free alloc = Anamnesis $ \ret env st c →
  unAnamnate alloc ret env st $ \case
    Left ex → c (Left ex)
    Right a → c (Right a) ⌦ \r → r ⚟ unAnamnate (free a) ret env st pure
{-# INLINE allocResource #-}
allocResource' ∷ (a → Anamnesis' e s ()) → Anamnesis r e s a → Anamnesis r e s (a, Anamnesis r e s ())
allocResource' free alloc = Anamnesis $ \ret env st c → unAnamnate alloc ret env st $ \case
  Left ex → c (Left ex)
  Right a → c (Right (a, Anamnesis $ \ret' env' st' c' → c' (Right ()) ⌦ \r → r ⚟ unAnamnate (free a) ret' env' st' pure))
liftIOWith ∷ ((a → IO (Either AExcept b)) → IO (Either AExcept b)) → (a → Anamnesis' e s b) → Anamnesis r e s b
liftIOWith iof pf = Anamnesis $ \ref env st c → iof (\a → unAnamnate (pf a) ref env st pure) ⌦ c
{-# INLINE liftIOWith #-}
-- dataframe pointers
allocaPeekDF ∷ ∀ a (ns ∷ [Nat]) r e s. (PrimBytes a, Dimensions ns) ⇒ (Ptr a → Anamnesis () e s ()) → Anamnesis r e s (DataFrame a ns)
allocaPeekDF pf
  | Dict ← inferKnownBackend @a @ns = Anamnesis $ \ret env st c → do
    mdf ← newPinnedDataFrame
    locVar ← liftIO newEmptyMVar
    withDataFramePtr mdf $ \ptr → unAnamnate (pf ptr) ret env st (putMVar locVar)
    df ← unsafeFreezeDataFrame mdf
    takeMVar locVar ⌦ c ∘ (df ⚟)
