module Epiklesis.Elems where
-- functions designed to create
-- ui elements easily
import Prelude ()
import UPrelude
import Data.List.Split (splitOn)
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Oblatum.Data
import Paracletus.Oblatum.Font

-- figure out what size the textbox should be
calcTextBoxSize ∷ TextSize → String → (Int,Int)
calcTextBoxSize size str = (max 0 (round (calcTBWidth size str)), (length (splitOn ['\n'] str)))
calcTBWidth ∷ TextSize → String → Double
calcTBWidth _    []       = -1.0
calcTBWidth size (ch:str) = chWidth + calcTBWidth size str
  where chWidth = 2.0*(chW $ indexTTF size ch)
-- create a textbox of arbitrary size
addTextBox ∷ TextSize → (Double,Double) → (Int,Int) → [GTile]
addTextBox size (x,y) (sx,sy) = [middleTile,rightTile,leftTile,topTile,bottomTile,topLeftTile,topRightTile,botLeftTile,botRightTile]
  where sx'          = case size of
                         TextSize16px → fromIntegral sx
                         TextSize30px → 2*fromIntegral sx
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

addTTF ∷ Bool → TextSize → Double → (Double,Double) → String → [GTile]
addTTF _ _    _  _     []         = []
addTTF t size x0 (_,y) ('\n':str) = addTTF t size x0 (x0,(y - 1)) str
addTTF t size x0 (x,y) (' ':str)  = addTTF t size x0 (x + 0.5, y) str
addTTF t size x0 (x,y) (ch:str)   = [textTile] ⧺ addTTF t size x0 (x + chX', y) str
  where textTile = GTileUncached (x + (chX' / 2.0),y + chY') (chW',chH') (0,0) (1,1) (chIndex) t False
        TTFData chIndex chW chH chX chY = indexTTF size ch
        chW' = case size of
                 TextSize16px → 0.5*chW
                 TextSize30px → chW
        chH' = case size of
                 TextSize16px → 0.5*chH
                 TextSize30px → chH
        chX' = case size of
                 TextSize16px → 0.5*chX
                 TextSize30px → chX
        chY' = case size of
                 TextSize16px → 0.5*chY
                 TextSize30px → chY

addText ∷ Bool → Double → (Double,Double) → String → [GTile]
addText _ _  _     []         = []
addText t x0 (_,y) ('\n':str) = addText t x0 (x0,(y - 1)) str
addText t x0 (x,y) (ch:str)   = [textTile] ⧺ addText t x0 (x + (fontOffset ch),y) str
  where textTile = GTileUncached (x,y) (1,1) (fontIndex ch) (16,6) 1 t False

-- menus are full of bits
calcMenu ∷ (Double,Double) → [MenuBit] → [GTile]
calcMenu pos mbs = addTextBox TextSize16px posOffset size ⧺ calcMenuBits pos mbs
  where size = (20,(2*(length mbs) - 1))
        posOffset = ((fst pos) - 1.0,(snd pos) + 0.5)
calcMenuBits ∷ (Double,Double) → [MenuBit] → [GTile]
calcMenuBits _   []       = []
calcMenuBits pos (mb:mbs) = calcMenuBit pos mb ⧺ calcMenuBits pos' mbs
  where pos' = (fst pos, (snd pos) - 1)
calcMenuBit ∷ (Double,Double) → MenuBit → [GTile]
calcMenuBit pos (MenuText str)          = addText False (fst pos) pos str
calcMenuBit pos (MenuSlider text range val sel) = [boxTile] ⧺ addText False (fst pos) pos ((justifylString 10 (text ⧺ ": ")) ⧺ (show (fst range)) ⧺ " <-------> " ⧺ (justifyr (snd range)) ⧺ " " ⧺ (justifyr val))
  where boxTile = GTileUncached (((fst pos) + 5.75),(snd pos)) (2.0,1.0) (0,0) (1,1) tex False False
        tex = case sel of
                True → 19
                False → 20
        justifyr n
          | n < 1000  = " " ⧺ (show n)
          | n < 100   = "  " ⧺ (show n)
          | n < 10    = "   " ⧺ (show n)
          | otherwise = show n
        justifylString n s = s ⧺ spaces
          where spaces = take (n - (length s)) $ repeat ' '
calcMenuBit _   MenuNULL                = []
