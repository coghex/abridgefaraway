module Paracletus.Draw where
-- generic epiklesis data is converted
-- to generic paracletus data, graphics
-- layer specific translations occur during
-- vertex translation in seperate modules
import Prelude()
import UPrelude
import Data.List (sortBy)
import Data.Function (on)
import Anamnesis.Map
import Epiklesis.Data
import Epiklesis.Elems
import Epiklesis.Shell
import Epiklesis.World
import Paracletus.Data

-- shell can be safely drawn over everything
loadDrawState ∷ LuaState → DrawState
loadDrawState ls = DrawState (tiles ⧺ shTiles) dynData
  where tiles   = loadWindow nDefTex currWin
        shTiles = genShell $ luaShell ls
        winInd  = luaCurrWin ls
        currWin = (luaWindows ls) !! winInd
        dynData = calcDynData currWin
        nDefTex = luaNDefTex ls
  
loadWindow ∷ Int → Window → [GTile]
loadWindow nDefTex win = loadWinElems nDefTex $ reverse $ mysort (zip (winCache win) (winElems win))
mysort ∷ Ord b ⇒ [(a,b)] → [(a,b)]
mysort = sortBy (flip compare `on` snd)

loadWinElems ∷ Int → ([(WinElemCache,WinElem)]) → [GTile]
loadWinElems _       []                    = []
loadWinElems nDefTex ((WECached gts,_):es) = gts ⧺ loadWinElems nDefTex es
loadWinElems nDefTex ((_,e):es)           = loadWinElem nDefTex e ⧺ loadWinElems nDefTex es

loadWinElem ∷ Int → WinElem → [GTile]
loadWinElem _       (WinElemText pos True  str) = (addTextBox posOffset size) ⧺ addText False (fst pos) pos str
  where size = calcTextBoxSize str
        posOffset = ((fst pos) - 1.0,(snd pos) + 0.5)
loadWinElem _       (WinElemText pos False str) = addText False (fst pos) pos str
loadWinElem _       (WinElemMenu _ pos bits) = calcMenu pos bits
loadWinElem nDefTex (WinElemBack _) = [GTileUncached (0,0) (32,24) (0,0) (1,1) nDefTex False False]
loadWinElem _       (WinElemWorld wp wd _) = calcTiles wp wd
loadWinElem _       (WinElemLink _ _ _)    = []
loadWinElem _       (WinElemDyn DynFPS _)  = calcFPSTiles
loadWinElem _       (WinElemDyn (DynSlider pos) _) = [GTileUncached pos (0.5,0.5) (8,5) (16,6) 1 True False]
loadWinElem _       (WinElemDyn DynNULL _) = []
loadWinElem _       WinElemNULL = []

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
                               , tTile = False
                               , tMoves = True }
          ix = (tileType gspot) `mod` 3
          iy = (tileType gspot) `div` 3


moveDynTile ∷ [DynData] → Int → Cardinal → Float → [DynData]
moveDynTile dd n card dist = moveDynTileF 0 dd n card dist
moveDynTileF ∷ Int → [DynData] → Int → Cardinal → Float → [DynData]
moveDynTileF _ []       _ _    _    = []
moveDynTileF i (dd:dds) n card dist = [dd'] ⧺ moveDynTileF (i+1) dds n card dist
  where dd'    = if (i ≡ n) then newdd else dd
        newdd  = dd { ddPosition = newPos }
        newPos = case card of
                   North → (x,y + dist)
                   South → (x,y - dist)
                   East  → (x + dist,y)
                   West  → (x - dist,y)
                   _     → (x,y)
        (x,y)  = ddPosition dd

-- returns dyn data structures from lua state
calcDynData ∷ Window → [DynData]
calcDynData win = calcDynDataElems $ winElems win
calcDynDataElems ∷ [WinElem] → [DynData]
calcDynDataElems []                      = []
calcDynDataElems ((WinElemDyn _ dd):wes) = dd ⧺ calcDynDataElems wes
calcDynDataElems (_:wes)                 = calcDynDataElems wes

-- calculates tiles for fps
calcFPSTiles ∷ [GTile]
calcFPSTiles = [tile1,tile2,tile3,tile4]
  where tile1 = GTileUncached (3,2)   (1,1) (15,4) (16,6) 1 True False
        tile2 = GTileUncached (3.3,2) (1,1) (15,4) (16,6) 1 True False
        tile3 = GTileUncached (3.6,2) (1,1) (15,4) (16,6) 1 True False
        tile4 = GTileUncached (3.9,2) (1,1) (15,4) (16,6) 1 True False

