module Anamnesis.Draw where
-- gl agnostic draw functions
-- take global draw state and
-- update it, or translate it
-- for the specific gl
import Prelude()
import UPrelude
import Graphics.Vulkan.Core_1_0
import Paracletus.Data

-- a data structure containing
-- the abstract representation
-- of the general vertex layout
data DrawState = DrawState { dsTiles ∷ [GTile]
                           , dsTextB ∷ [TextBox]
                           , dsMBox  ∷ MouseBox } deriving (Show, Eq)
-- defines a box full of text
data TextBox = TextBox { tbPos    ∷ (Double,Double)
                       , tbSize   ∷ (Int, Int)
                       , tbBox    ∷ Bool
                       , tbString ∷ String } deriving (Show, Eq)

data MouseBox = MBNULL | MouseBox { mbPos1 ∷ (Float,Float)
                                  , mbPos2 ∷ (Float,Float) } deriving (Show, Eq)

initDrawState ∷ [GTile] → IO DrawState
initDrawState tiles = return $ DrawState
  { dsTiles = tiles 
  , dsTextB = [initTB]
  , dsMBox = MBNULL }
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
                  , tT     = 2 } ]
