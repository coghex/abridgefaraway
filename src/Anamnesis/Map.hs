module Anamnesis.Map where
-- 2 dimensional related
-- helper functions occur
import Prelude()
import UPrelude

data Cardinal = North | South | West | East | CardNULL

moveCursor ∷ (Num a) ⇒ a → (a, a, a) → Cardinal → (a, a, a)
moveCursor step (x,y,z) North    = (x,y+step, z)
moveCursor step (x,y,z) South    = (x,y-step, z)
moveCursor step (x,y,z) East     = (x+step,y, z)
moveCursor step (x,y,z) West     = (x-step,y, z)
moveCursor _    (x,y,z) CardNULL = (x,y,z)
transCursor ∷ (Num a) ⇒ (a, a, a) → (a, a, a) → (a, a, a)
transCursor (bx, by, bz) (cx, cy, cz) = (bx+cx, by+cy, bz+cz)
