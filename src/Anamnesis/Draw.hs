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
data DrawState = DrawState { dsCmdBP ∷ Ptr VkCommandBuffer
                           , dsTiles ∷ [GTile]
                           , dsTextB ∷ [TextBox] }
-- defines a box full of text
data TextBox = TextBox { tbPos ∷ (Int, Int)
                       , tbString ∷ String }

initDrawState ∷ [GTile] → IO DrawState
initDrawState tiles = return $ DrawState
  { dsCmdBP = VK_NULL
  , dsTiles = tiles 
  , dsTextB = [initTB] }
  where initTB = TextBox { tbPos = (-1,-1)
                         , tbString = "loading..." }

addTile ∷ DrawState → DrawState
addTile ds = ds { dsTiles = newtiles }
  where newtiles = (dsTiles ds) ⧺
          [ GTile { tPos  = (3,0)
                  , tInd  = (2,0)
                  , tSize = (16,6)
                  , tT    = 2 } ]
