module Anamnesis.Draw where
-- gl agnostic draw functions
-- take global draw state and
-- update it, or translate it
-- for the specific gl
import Prelude()
import UPrelude
import Anamnesis.Data
import Paracletus.Data
import Epiklesis.Data

initDrawState ∷ [GTile] → IO DrawState
initDrawState tiles = return $ DrawState
  { dsTiles = tiles 
  , dsTextB = [initTB]
  , dsMBox = MBNULL
  , dsShell = ShellNULL }
  where initTB = TextBox { tbPos    = (-1,-1)
                         , tbSize   = (8,2)
                         , tbBox    = True
                         , tbString = str }
        str = "loading...\nblop blop"

addTile ∷ DrawState → DrawState
addTile ds = ds { dsTiles = newtiles }
  where newtiles = (dsTiles ds) ⧺
          [ GTile { tPos   = (3,0)
                  , tScale = (1,1)
                  , tInd   = (2,0)
                  , tSize  = (16,6)
                  , tT     = 2
                  , tMoves = False } ]
