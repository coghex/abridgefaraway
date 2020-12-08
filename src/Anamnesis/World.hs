{-# LANGUAGE Strict #-}
module Anamnesis.World where
-- various functions to make lua world
-- gen faster
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Lua
import Epiklesis.World

createWorld ∷ Int → Int → Int → Int → String → World
createWorld sw sh zw zh texs = World [initZone] (sw,sh) texs
  where initZone = Zone (0,0) $ take zh (repeat (take zw (repeat (seg))))
        seg = SegmentNULL--Segment $ take sh $ repeat $ take sw $ repeat $ Tile 1 1

-- called from loading thread, fills
-- out the winElemWorld
loadWorld ∷ LuaState → LuaState
loadWorld ls = case (findWorldData (currentWindow ls)) of
  Just (wp,wd) → ls { luaWindows = newWins }
    where newWins      = findAndReplaceWindow newWin $ luaWindows ls
          newWin       = replaceWorldData (currentWindow ls) newWorldData
          newWorldData = findAndReplaceSegments (wpZSize wp) zoneInd newSegs wd
          newSegs      = genSegs $ evalScreenCursor sc
          sc           = ((wdCam wd),(wdCSize wd))
          -- this is temporary
          zoneInd      = (0,0)
  Nothing      → ls

-- returns the list of indecies
-- of segments to generate
evalScreenCursor ∷ ((Float,Float),(Int,Int)) → [(Int,Int)]
evalScreenCursor ((cx,cy),_) = [pos]
  where pos = (x,y)
        x = floor $ cx / 16
        y = floor $ cy / 8

-- generates the segments that are
-- required by evalScreenCursor
genSegs ∷ [(Int,Int)] → [((Int,Int),Segment)]
genSegs []             = []
genSegs (pos:poss) = [(pos,seg)] ⧺ (genSegs poss)
  where seg = Segment $ take 8 (repeat (take 16 (repeat (Tile 2 1))))

-- returns the first world element
-- found on the current window
findWorldData ∷ Window → (Maybe (WorldParams,WorldData))
findWorldData (Window _ _ _ elems _) = findElemData elems
findElemData ∷ [WinElem] → (Maybe (WorldParams,WorldData))
findElemData []                            = Nothing
findElemData ((WinElemNULL):elems)         = findElemData elems
findElemData ((WinElemText _ _ _):elems)   = findElemData elems
findElemData ((WinElemMenu _ _ _):elems)   = findElemData elems
findElemData ((WinElemDyn _ _):elems)      = findElemData elems
findElemData ((WinElemBack _):elems)       = findElemData elems
findElemData ((WinElemLink _ _ _):elems)   = findElemData elems
findElemData ((WinElemWorld wp wd _):_) = Just (wp,wd)

-- replaces world data in a window
replaceWorldData ∷ Window → WorldData → Window
replaceWorldData (Window name wType curs elems cache) wd = Window name wType curs (map (replaceElemData wd) elems) (reloadcache)
  where reloadcache = take (length cache) (repeat WEUncached)
replaceElemData ∷ WorldData → WinElem → WinElem
replaceElemData _   (WinElemNULL)           = WinElemNULL
replaceElemData _   (WinElemText tp tb ts)  = WinElemText tp tb ts
replaceElemData _   (WinElemMenu mn mp mb)  = WinElemMenu mn mp mb
replaceElemData wd' (WinElemWorld wp _ wt) = WinElemWorld wp wd' wt
replaceElemData _   (WinElemLink lp lb la)  = WinElemLink lp lb la
replaceElemData _   (WinElemBack fp)        = WinElemBack fp
replaceElemData _   (WinElemDyn dt dd)      = WinElemDyn dt dd

-- finds and replaces mulitiple segemts
-- in world data, segments zipped with inds
findAndReplaceSegments ∷ (Int,Int) → (Int,Int) → [((Int,Int),Segment)] → WorldData → WorldData
findAndReplaceSegments _        _       []         wd = wd
findAndReplaceSegments zoneSize zoneInd ((segInd,seg):segs) wd = findAndReplaceSegments zoneSize zoneInd (segs) newWD
  where newWD = findAndReplaceSegment zoneSize zoneInd segInd seg wd

-- finds a segment in world data,
-- then replace it with a new segment.
-- since zones can be in any order, we
-- need to go through the list twice to
-- see if we need to generate a new zone
findAndReplaceSegment ∷ (Int,Int) → (Int,Int) → (Int,Int) → Segment → WorldData → WorldData
findAndReplaceSegment zoneSize zoneInd segInd newSeg (WorldData cam camSize zones) = WorldData cam camSize zones'
  where zones' = case (zoneExists zoneInd zones) of
          True  → findAndReplaceZone zoneSize zoneInd segInd newSeg zones
          False → zones ⧺ [(Zone zoneInd [])]
findAndReplaceZone ∷ (Int,Int) → (Int,Int) → (Int,Int) → Segment → [Zone] → [Zone]
findAndReplaceZone _        _       _      _      [] = []
findAndReplaceZone zoneSize testInd segInd newSeg ((ZoneNULL):zs) = [ZoneNULL] ⧺ findAndReplaceZone zoneSize testInd segInd newSeg zs
findAndReplaceZone zoneSize testInd segInd newSeg ((Zone ind segs):zs)
  | (testInd ≡ ind) = [Zone ind segs'] ⧺ findAndReplaceZone zoneSize testInd segInd newSeg zs
  | otherwise       = [Zone ind segs]  ⧺ findAndReplaceZone zoneSize testInd segInd newSeg zs
  where segs' = findAndReplaceSegmentSpot zoneSize segInd segs newSeg
findAndReplaceSegmentSpot ∷ (Int,Int) → (Int,Int) → [[Segment]] → Segment → [[Segment]]
findAndReplaceSegmentSpot zoneSize segInd segs newSeg = map (findAndReplaceSegmentRow zoneSize segInd newSeg) (zip yinds segs)
  where yinds = take (fst zoneSize) [0..]
findAndReplaceSegmentRow ∷ (Int,Int) → (Int,Int) → Segment → (Int,[Segment]) → [Segment]
findAndReplaceSegmentRow zoneSize segInd newSeg (j,segs) = map (findAndReplaceSegmentSpotSpot segInd newSeg j) (zip xinds segs)
  where xinds = take (snd zoneSize) [0..]
findAndReplaceSegmentSpotSpot ∷ (Int,Int) → Segment → Int → (Int,Segment) → Segment
findAndReplaceSegmentSpotSpot segInd newSeg j (i,seg)
  | (i,j) ≡ segInd = newSeg
  | otherwise      = seg

-- tells us if a zone is loaded in,
-- null zones dont count
zoneExists ∷ (Int,Int) → [Zone] → Bool
zoneExists _       [] = False
zoneExists testInd ((ZoneNULL):zs) = zoneExists testInd zs
zoneExists testInd ((Zone ind _):zs)
  | (testInd ≡ ind) = True
  | otherwise       = zoneExists testInd zs

findAndReplaceWindow ∷ Window → [Window] → [Window]
findAndReplaceWindow _ [] = []
findAndReplaceWindow (Window testname a1 a2 a3 a4) ((Window name b1 b2 b3 b4):ws)
  | testname ≡ name = [Window testname a1 a2 a3 a4] ⧺ findAndReplaceWindow (Window testname a1 a2 a3 a4) ws
  | otherwise       = [Window name     b1 b2 b3 b4] ⧺ findAndReplaceWindow (Window testname a1 a2 a3 a4) ws
