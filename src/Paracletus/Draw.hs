module Paracletus.Draw where
-- generic epiklesis data is converted
-- to generic paracletus data, graphics
-- layer specific translations occur during
-- vertex translation in seperate modules
import Prelude()
import UPrelude
import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Function (on)
import Anamnesis.Data
import Epiklesis.Data
import Epiklesis.Elems
import Epiklesis.Shell
import Epiklesis.World
import Paracletus.Data
import Paracletus.Oblatum.Data

-- shell can be safely drawn over everything
loadDrawState ∷ LuaState → DrawState
loadDrawState ls = DrawState (tiles ⧺ shTiles)
  where tiles   = loadWindow currWin
        shTiles = genShell $ luaShell ls
        winInd  = luaCurrWin ls
        currWin = (luaWindows ls) !! winInd
  
loadWindow ∷ Window → [GTile]
loadWindow win = loadWinElems $ reverse $ mysort (zip (winCache win) (winElems win))
mysort ∷ Ord b ⇒ [(a,b)] → [(a,b)]
mysort = sortBy (flip compare `on` snd)

loadWinElems ∷ ([(WinElemCache,WinElem)]) → [GTile]
loadWinElems []                    = []
loadWinElems ((WECached gts,e):es) = gts ⧺ loadWinElems es
loadWinElems ((ec,e):es)           = loadWinElem e ⧺ loadWinElems es

loadWinElem ∷ WinElem → [GTile]
loadWinElem (WinElemText pos True  str) = (addTextBox posOffset size) ⧺ addText (fst pos) pos str
  where size = calcTextBoxSize str
        posOffset = ((fst pos) - 1.0,(snd pos) + 0.5)
loadWinElem (WinElemText pos False str) = addText (fst pos) pos str
loadWinElem (WinElemFPS fps) = addText 0 (0,0) (show fps)
loadWinElem (WinElemBack _ ) = [GTileUncached (0,0) (32,24) (0,0) (1,1) 19 False]
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
calcSegRow (cx,cy) (cw,ch) (x,y) (grow:grows) = [rowTiles] ⧺ (calcSegRow (cx,cy) (cw,ch) (x,(y + 1)) grows)
    where rowTiles = calcSegSpot (cx,cy) (cw,ch) (x,y) grow

calcSegSpot ∷ (Int,Int) → (Int,Int) → (Int,Int) → [Tile] → [GTile]
calcSegSpot _       _       _     [] = []
calcSegSpot (cx,cy) (cw,ch) (x,y) (gspot:gspots) = [tile] ⧺ (calcSegSpot (cx,cy) (cw,ch) ((x + 1),y) gspots)
    where tile = GTileUncached { tPos = (((fromIntegral x) - 1.0), ((fromIntegral y) - 1.0))
                               , tScale = (1,1)
                               , tInd = (ix,iy)
                               , tSize = (3,15)
                               , tT = 20
                               , tMoves = True }
          ix = (tileType gspot) `mod` 3
          iy = (tileType gspot) `div` 3


