{-# LANGUAGE Strict #-}
module Anamnesis.Util where
import Prelude()
import UPrelude
-- function to help with pointers
allocResource ∷ (a → Anamnesis' r e s ()) → Anamnesis r e s a → Anamnesis r e s a
allocResource free alloc = Anamnesis $ \ret env state c →
  unAnamnate alloc ref $ \case
    Left  e → c (Left  e)
    Right a → c (Right a) ⌦ \r → r <$ unAnamnate (free a) ref pure
{-# INLINE allocResource #-}
