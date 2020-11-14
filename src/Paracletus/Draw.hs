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
import Epiklesis.World
import Paracletus.Data
import Paracletus.Oblatum.Data

loadDrawState ∷ LuaState → DrawState
loadDrawState ls = DrawState tiles
  where tiles   = loadWindow currWin
        winInd  = luaCurrWin ls
        currWin = (luaWindows ls) !! winInd
  
loadWindow ∷ Window → [GTile]
loadWindow win = loadWinElems $ sort $ winElems win

loadWinElems ∷ [WinElem] → [GTile]
loadWinElems []           = []
loadWinElems (e:es) = loadWinElem e ⧺ loadWinElems es

loadWinElem ∷ WinElem → [GTile]
loadWinElem (WinElemText pos True  str) = (addTextBox posOffset size) ⧺ addText pos str
  where size = calcTextBoxSize str
        posOffset = ((fst pos) - 1.0,(snd pos) + 0.5)
loadWinElem (WinElemText pos False str) = addText pos str
loadWinElem (WinElemBack _ ) = [GTile (0,0) (32,24) (0,0) (1,1) 19 False]
loadWinElem (WinElemWorld wp wd _) = calcTiles wp wd--[GTile (0,0) (1,1) (0,0) (3,15) 20 True]
loadWinElem (WinElemLink _ _ _) = []
loadWinElem WinElemNULL = []

-- converts tiles in world data into GTile list
calcTiles ∷ WorldParams → WorldData → [GTile]
calcTiles _  (WorldData _   _       [])     = []
calcTiles wp (WorldData cam camSize (z:zs)) = (calcZoneTiles wp cam camSize z) ⧺ (calcTiles wp (WorldData cam camSize zs))

calcZoneTiles ∷ WorldParams → (Float,Float) → (Int,Int) → Zone → [GTile]
calcZoneTiles _  _   _       (ZoneNULL)      = []
calcZoneTiles wp cam camSize (Zone ind segs) = flatten $ map (calcZoneRows wp cam camSize ind) (zip yinds segs)
  where yinds = take (fst segSize) [0..]
        segSize = wpZSize wp

calcZoneRows ∷ WorldParams → (Float,Float) → (Int,Int) → (Int,Int) → (Integer,[Segment]) → [GTile]
calcZoneRows wp cam camSize ind (j,segs) = flatten $ map (calcZoneSpot j' wp cam camSize ind) (zip xinds segs)
  where xinds = take (snd segSize) [0..]
        segSize = wpZSize wp
        j' = fromIntegral j

calcZoneSpot ∷ Int → WorldParams → (Float,Float) → (Int,Int) → (Int,Int) → (Integer,Segment) → [GTile]
calcZoneSpot j wp cam camSize ind (i,seg) = calcSegTiles (i',j) wp roundCam camSize ind seg
  where roundCam = ((round (fst cam)),(round (snd cam)))
        i' = fromIntegral i

calcSegTiles ∷ (Int,Int) → WorldParams → (Int,Int) → (Int,Int) → (Int,Int) → Segment → [GTile]
calcSegTiles _     _  _   _       _   (SegmentNULL) = []
calcSegTiles (i,j) wp cam camSize ind (Segment grid) = flatten $ calcSegRow cam camSize (x,y) grid
  where (x,y) = (sw*(i + (fst ind)),sh*(j + (snd ind)))
        (sw,sh) = wpSSize wp

calcSegRow ∷ (Int,Int) → (Int,Int) → (Int,Int) → [[Tile]] → [[GTile]]
calcSegRow _       _       _     [[]]         = [[]]
calcSegRow _       _       _     []           = [[]]
calcSegRow (cx,cy) (cw,ch) (x,y) (grow:grows)
  | ((y > (cy + ch)) ∨ (y < (cy - ch))) = calcSegRow (cx,cy) (cw,ch) (x,(y + 1)) grows
  | otherwise = [rowTiles] ⧺ (calcSegRow (cx,cy) (cw,ch) (x,(y + 1)) grows)
    where rowTiles = calcSegSpot (cx,cy) (cw,ch) (x,y) grow

calcSegSpot ∷ (Int,Int) → (Int,Int) → (Int,Int) → [Tile] → [GTile]
calcSegSpot _       _       _     [] = []
calcSegSpot (cx,cy) (cw,ch) (x,y) (gspot:gspots)
  | ((x > (cx + cw)) ∨ (x < (cx - cw))) = calcSegSpot (cx,cy) (cw,ch) ((x + 1),y) gspots
  | otherwise = [tile] ⧺ (calcSegSpot (cx,cy) (cw,ch) ((x + 1),y) gspots)
    where tile = GTile { tPos = (((fromIntegral x) - 1.0), ((fromIntegral y) - 1.0))
                       , tScale = (1,1)
                       , tInd = (ix,iy)
                       , tSize = (3,15)
                       , tT = 20
                       , tMoves = True }
          ix = (tileType gspot) `mod` 3
          iy = (tileType gspot) `div` 3

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

flatten ∷ [[α]] → [α]
flatten xs = (\z n → foldr (\x y → foldr z y x) n xs) (:) []
