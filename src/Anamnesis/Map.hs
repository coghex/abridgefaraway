module Anamnesis.Map where
-- 2 dimensional related
-- helper functions occur
import Prelude()
import UPrelude

data Cardinal = North | South | West | East

moveCursor ∷ Int → (Int, Int) → Cardinal → (Int, Int)
moveCursor step (x,y) North = (x,y+step)
moveCursor step (x,y) South = (x,y-step)
moveCursor step (x,y) East = (x+step,y)
moveCursor step (x,y) West = (x-step,y)
