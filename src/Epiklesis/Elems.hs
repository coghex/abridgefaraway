module Epiklesis.Elems where
-- functions designed to create
-- ui elements easily
import Prelude ()
import UPrelude
import Data.List.Split (splitOn)
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
        topLeftTile  = defaultGTile { tPos   = (x,y)
                                    , tScale = (0.5,0.5)
                                    , tT     = 6 }
        topRightTile = defaultGTile { tPos   = (x + (0.5*sx') + 0.5,y)
                                    , tScale = (0.5,0.5)
                                    , tT     = 5 }
        botLeftTile  = defaultGTile { tPos   = (x,y - (0.5*sy') - 0.5)
                                    , tScale = (0.5,0.5)
                                    , tT     = 9 }
        botRightTile = defaultGTile { tPos   = (x + (0.5*sx') + 0.5, y - (0.5*sy') - 0.5)
                                    , tScale = (0.5,0.5)
                                    , tT     = 8 }
        topTile      = defaultGTile { tPos   = (x + (0.25*sx') + 0.25,y)
                                    , tScale = ((0.5*sx'),0.5)
                                    , tT     = 4 }
        leftTile     = defaultGTile { tPos   = (x,y - (0.25*sy') - 0.25)
                                    , tScale = (0.5,(0.5*sy'))
                                    , tT     = 10 }
        rightTile    = defaultGTile { tPos   = (x + (0.5*sx') + 0.5,y - (0.25*sy') - 0.25)
                                    , tScale = (0.5,(0.5*sy'))
                                    , tT     = 3 }
        bottomTile   = defaultGTile { tPos   = (x + (0.25*sx') + 0.25,y - (0.5*sy') - 0.5)
                                    , tScale = ((0.5*sx'),0.5)
                                    , tT     = 7 }
        middleTile   = defaultGTile { tPos   = (x + (0.25*sx') + 0.25,y - (0.25*sy') - 0.25)
                                    , tScale = (0.5*sx',0.5*sy')
                                    , tT     = 2 }


addText ∷ Double → (Double,Double) → String → [GTile]
addText _  _     []         = []
addText x0 (x,y) ('\n':str) = addText x0 (x0,(y - 1)) str
addText x0 (x,y) (ch:str)   = [textTile] ⧺ addText x0 (x + (fontOffset ch),y) str
  where textTile = GTileUncached (x,y) (1,1) (fontIndex ch) (16,6) 1 False

