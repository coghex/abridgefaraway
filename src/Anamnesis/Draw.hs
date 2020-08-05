module Anamnesis.Draw where
-- gl agnostic draw functions
-- take global draw state and
-- update it, or translate it
-- for the specific gl
import Prelude()
import UPrelude
import Paracletus.Data

data DrawState = DrawState { dsTiles ∷ [GTile] }

initDrawState ∷ [GTile] → IO DrawState
initDrawState tiles = return $ DrawState { dsTiles = tiles }

addTile ∷ DrawState → DrawState
addTile ds = DrawState { dsTiles = newtiles }
  where newtiles = (dsTiles ds) ⧺
          [ GTile { tPos  = (3,0)
                  , tInd  = (2,0)
                  , tSize = (16,6)
                  , tT    = 2 } ]
