module Epiklesis.Elems where
-- functions designed to create
-- ui elements easily
import Prelude ()
import UPrelude
import Data.List.Split (splitOn)
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Oblatum.Data

-- figure out what size the textbox should be
calcTextBoxSize ∷ String → (Int,Int)
calcTextBoxSize str = (length str, (length (splitOn ['\n'] str)))
-- create a textbox of arbitrary size
addTextBox ∷ (Double,Double) → (Int,Int) → [GTile]
addTextBox (x,y) (sx,sy) = [middleTile,rightTile,leftTile,topTile,bottomTile,topLeftTile,topRightTile,botLeftTile,botRightTile]
  where sx'          = fromIntegral sx
        sy'          = fromIntegral sy
        topLeftTile  = GTileUncached { tPos   = (x,y)
                                     , tInd   = (0,0)
                                     , tSize  = (1,1)
                                     , tScale = (0.5,0.5)
                                     , tT     = 6
                                     , tTile  = False
                                     , tMoves = False }
        topRightTile = GTileUncached { tPos   = (x + (0.5*sx') + 0.5,y)
                                     , tInd   = (0,0)
                                     , tSize  = (1,1)
                                     , tScale = (0.5,0.5)
                                     , tT     = 5
                                     , tTile  = False
                                     , tMoves = False }
        botLeftTile  = GTileUncached { tPos   = (x,y - (0.5*sy') - 0.5)
                                     , tInd   = (0,0)
                                     , tSize  = (1,1)
                                     , tScale = (0.5,0.5)
                                     , tT     = 9
                                     , tTile  = False
                                     , tMoves = False }
        botRightTile = GTileUncached { tPos   = (x + (0.5*sx') + 0.5, y - (0.5*sy') - 0.5)
                                     , tInd   = (0,0)
                                     , tSize  = (1,1)
                                     , tScale = (0.5,0.5)
                                     , tT     = 8
                                     , tTile  = False
                                     , tMoves = False }
        topTile      = GTileUncached { tPos   = (x + (0.25*sx') + 0.25,y)
                                     , tInd   = (0,0)
                                     , tSize  = (1,1)
                                     , tScale = ((0.5*sx'),0.5)
                                     , tT     = 4
                                     , tTile  = False
                                     , tMoves = False }
        leftTile     = GTileUncached { tPos   = (x,y - (0.25*sy') - 0.25)
                                     , tInd   = (0,0)
                                     , tSize  = (1,1)
                                     , tScale = (0.5,(0.5*sy'))
                                     , tT     = 10
                                     , tTile  = False
                                     , tMoves = False }
        rightTile    = GTileUncached { tPos   = (x + (0.5*sx') + 0.5,y - (0.25*sy') - 0.25)
                                     , tInd   = (0,0)
                                     , tSize  = (1,1)
                                     , tScale = (0.5,(0.5*sy'))
                                     , tT     = 3
                                     , tTile  = False
                                     , tMoves = False }
        bottomTile   = GTileUncached { tPos   = (x + (0.25*sx') + 0.25,y - (0.5*sy') - 0.5)
                                     , tInd   = (0,0)
                                     , tSize  = (1,1)
                                     , tScale = ((0.5*sx'),0.5)
                                     , tT     = 7
                                     , tTile  = False
                                     , tMoves = False }
        middleTile   = GTileUncached { tPos   = (x + (0.25*sx') + 0.25,y - (0.25*sy') - 0.25)
                                     , tScale = (0.5*sx',0.5*sy')
                                     , tInd   = (0,0)
                                     , tSize  = (1,1)
                                     , tT     = 2
                                     , tTile  = False
                                     , tMoves = False }


addText ∷ Bool → Double → (Double,Double) → String → [GTile]
addText _ _  _     []         = []
addText t x0 (_,y) ('\n':str) = addText t x0 (x0,(y - 1)) str
addText t x0 (x,y) (ch:str)   = [textTile] ⧺ addText t x0 (x + (fontOffset ch),y) str
  where textTile = GTileUncached (x,y) (1,1) (fontIndex ch) (16,6) 1 t False

-- menus are full of bits
calcMenu ∷ (Double,Double) → [MenuBit] → [GTile]
calcMenu pos mbs = addTextBox posOffset size ⧺ calcMenuBits pos mbs
  where size = (20,(2*(length mbs) - 1))
        posOffset = ((fst pos) - 1.0,(snd pos) + 0.5)
calcMenuBits ∷ (Double,Double) → [MenuBit] → [GTile]
calcMenuBits _   []       = []
calcMenuBits pos (mb:mbs) = calcMenuBit pos mb ⧺ calcMenuBits pos' mbs
  where pos' = (fst pos, (snd pos) - 1)
calcMenuBit ∷ (Double,Double) → MenuBit → [GTile]
calcMenuBit pos (MenuText str)          = addText False (fst pos) pos str
calcMenuBit pos (MenuSlider text range) = addText False (fst pos) pos (text ⧺ ": " ⧺ (show (fst range)) ⧺ " <-------> " ⧺ (show (snd range)))
calcMenuBit _   MenuNULL                = []
