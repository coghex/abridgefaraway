{-# LANGUAGE Strict #-}
module Anamnesis.Foreign where
-- a collection of memory operations
import Prelude()
import UPrelude
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import Foreign.Storable (Storable)
import Foreign.Ptr
import qualified Foreign.Storable as Storable
import Anamnesis
import Artos.Except
-- various pointer operations
alloca ∷ Storable a ⇒ (Ptr a → Anamnesis' e s b) → Anamnesis r e s b
alloca = liftIOWith Foreign.alloca
allocaPeek ∷ Storable a ⇒ (Ptr a → Anamnesis (Either AExcept a) e s ()) → Anamnesis r e s a
allocaPeek f = alloca $ \ptr → f ptr >> liftIO (Storable.peek ptr)
allocaArray ∷ Storable a ⇒ Int → (Ptr a → Anamnesis' e s b) → Anamnesis r e s b
allocaArray = liftIOWith . Foreign.allocaArray
newArrayRes ∷ Storable a ⇒ [a] → Anamnesis r e s (Ptr a)
newArrayRes xs = Anamnesis $ \_ _ _ c → Foreign.withArray xs (c . Right)
-- functions to help with pointers
allocResource ∷ (a → Anamnesis' e s ()) → Anamnesis r e s a → Anamnesis r e s a
allocResource free alloc = Anamnesis $ \ret env st c →
  unAnamnate alloc ret env st $ \case
    Left ex → c (Left ex)
    Right a → c (Right a) ⌦ \r → r <$ unAnamnate (free a) ret env st pure
{-# INLINE allocResource #-}
liftIOWith ∷ ((a → IO (Either AExcept b)) → IO (Either AExcept b)) → (a → Anamnesis' e s b) → Anamnesis r e s b
liftIOWith iof pf = Anamnesis $ \ref env st c → iof (\a → unAnamnate (pf a) ref env st pure) ⌦ c
{-# INLINE liftIOWith #-}
