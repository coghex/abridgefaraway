module Paracletus.Draw where
-- generic epiklesis data is converted
-- to generic paracletus data, graphics
-- layer specific translations occur during
-- vertex translation in seperate modules
import Prelude()
import UPrelude
import Data.List (sort)
import Data.List.Split (splitOn)
import Anamnesis.Data
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Oblatum.Data

loadDrawState ∷ LuaState → DrawState
loadDrawState ls = DrawState tiles
  where tiles   = loadWindow currWin
        winInd  = luaCurrWin ls
        currWin = (luaWindows ls) !! winInd
  
loadWindow ∷ Window → [GTile]
loadWindow win = loadWinElems $ winElems win

loadWinElems ∷ [WinElem] → [GTile]
loadWinElems []           = []
loadWinElems (e:es) = loadWinElem e ⧺ loadWinElems es

loadWinElem ∷ WinElem → [GTile]
loadWinElem (WinElemText pos True  str) = (addTextBox posOffset size) ⧺ addText pos str
  where size = calcTextBoxSize str
        posOffset = ((fst pos) - 1.0,(snd pos) + 0.5)
loadWinElem (WinElemText pos False str) = addText pos str
loadWinElem (WinElemBack fp) = [GTile (0,0) (32,24) (0,0) (1,1) 19 False]
loadWinElem (WinElemWorld fp) = [GTile (0,0) (1,1) (0,0) (3,15) 20 True]
loadWinElem (WinElemLink _ _ _) = []
loadWinElem WinElemNULL = []

addText ∷ (Double,Double) → String → [GTile]
addText _     []     = []
addText (x,y) (ch:str) = [textTile] ⧺ addText (x + (fontOffset ch),y) str
  where textTile = GTile (x,y) (1,1) (fontIndex ch) (16,6) 1 False

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
