{-# LANGUAGE Strict #-}
module Anamnesis.World where
-- various functions to make lua world
-- gen faster
import Prelude()
import UPrelude
import System.Random
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
          newSegs      = genSegs wpGen $ evalScreenCursor (w,h) sc
          (w,h)        = wpSSize wp
          wpGen        = genWorldParams wp
          sc           = ((wdCam wd),(wdCSize wd))
          -- this is temporary
          zoneInd      = (0,0)
  Nothing      → ls

-- returns the list of indecies
-- of segments to generate
evalScreenCursor ∷ (Int,Int) → ((Float,Float),(Int,Int)) → [(Int,Int)]
evalScreenCursor (w,h) ((cx,cy),_) = [pos,posn,poss,pose,posw,posnw,posne,posse,possw]
  where pos   = (x,y)
        posn  = (x,y + 1)
        poss  = (x,y - 1)
        posw  = (x - 1,y)
        pose  = (x + 1,y)
        posnw = (x - 1,y - 1)
        posne = (x + 1,y - 1)
        possw = (x - 1,y + 1)
        posse = (x + 1,y + 1)
        x     = floor $ cx / w'
        y     = floor $ cy / h'
        w'    = fromIntegral w
        h'    = fromIntegral h

-- creates rands out of world params
genWorldParams ∷ WorldParams → WorldParams
genWorldParams wp = wp { wpRands = rands
                       , wpConts = conts }
  where rands   = genRands sg0 sg1 ncont w h
        conts   = genConts sg0 sg1 ncont
        sg0     = (wpStdGs wp) !! 0
        sg1     = (wpStdGs wp) !! 1
        (w,h)   = ((fst (wpSSize wp))*w', (snd (wpSSize wp))*h')
        (w',h') = case (wpUWP wp) of
                  Nothing  → (10,8)
                  Just uwp → (uwpWidth uwp, uwpHeight uwp)
        ncont = case (wpUWP wp) of
                  Nothing  → 1
                  Just uwp → uwpNConts uwp

genRands ∷ StdGen → StdGen → Int → Int → Int → [((Int,Int),(Int,Int))]
genRands sg0 sg1 n w h = buildList2 (xl,yl)
  where xl  = buildList2 (xxl,xyl)
        yl  = buildList2 (yxl,yyl)
        xxl = randomList (0,w) n sg0
        xyl = randomList (0,h) n sg0
        yxl = randomList (0,w) n sg1
        yyl = randomList (0,h) n sg1
genConts ∷ StdGen → StdGen → Int → [(Int,Int)]
genConts sg0 sg1 n = buildList2 (xl,yl)
  where xl = randomList (1,30) n sg0
        yl = randomList (1,30) n sg1

randomList ∷ (Random α) ⇒ (α,α) → Int → StdGen → [α]
randomList bnds n = do
  take n ∘ randomRs bnds

buildList2 ∷ ([α],[α]) → [(α,α)]
buildList2 (_,[]) = []
buildList2 ([],_) = []
buildList2 (a:as,b:bs) = [(a,b)] ⧺ buildList2 (as,bs)

-- generates the segments that are
-- required by evalScreenCursor
genSegs ∷ WorldParams → [(Int,Int)] → [((Int,Int),Segment)]
genSegs _  []             = []
genSegs wp (pos:poss) = [(pos,seg)] ⧺ (genSegs wp poss)
  where seg = Segment $ seedSeg wp pos

-- generates tile list for single segment
seedSeg ∷ WorldParams → (Int,Int) → [[Tile]]
seedSeg wp pos = seedConts (sw,sh) pos conts rands zeroSeg
  where rands   = wpRands wp
        conts   = wpConts wp
        zeroSeg = take sh (zip [0..] (repeat (take sw (zip [0..] (repeat (Tile 2 2))))))
        (sw,sh) = wpSSize wp
seedConts ∷ (Int,Int) → (Int,Int) → [(Int,Int)] → [((Int,Int),(Int,Int))] → [(Int,[(Int,Tile)])] → [[Tile]]
seedConts _    _   _      []     seg = flattenSeg seg
seedConts _    _   []     _      seg = flattenSeg seg
seedConts size pos (c:cs) (r:rs) seg = seedConts size pos cs rs seg'
  where seg' = seedCont size pos c r seg
flattenSeg ∷ [(Int,[(Int,Tile)])] → [[Tile]]
flattenSeg [] = []
flattenSeg ((_,row):gs) = [flattenRow row] ⧺ flattenSeg gs
flattenRow ∷ [(Int,Tile)] → [Tile]
flattenRow [] = []
flattenRow ((_,g):gs) = [g] ⧺ flattenRow gs
seedCont ∷ (Int,Int) → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,[(Int,Tile)])] → [(Int,[(Int,Tile)])]
seedCont size pos conts rands seg = map (seedTileRow size pos conts rands) seg
seedTileRow ∷ (Int,Int) → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → (Int,[(Int,Tile)]) → (Int,[(Int,Tile)])
seedTileRow size pos conts rands (j,row) = (j,map (seedTile size pos conts rands j) row)
seedTile ∷ (Int,Int) → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → Int → (Int,Tile) → (Int,Tile)
seedTile (width,height) pos (_,s) ((w,x),(y,z)) j (i,t)
  | seedDistance i' j' w x y z < (s*maxsize) = (i,t')
  | otherwise                                = (i,t)
  where t'      = Tile 3 1
        i'      = i + ((fst pos)*width)
        j'      = j + ((snd pos)*height)
        maxsize = (max width height)*(max width height)

seedDistance ∷ Int → Int → Int → Int → Int → Int → Int
seedDistance x1 y1 x2 y2 x3 y3 = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  p1*p2

-- returns the first world element
-- found on the current window
findWorldData ∷ Window → (Maybe (WorldParams,WorldData))
findWorldData (Window _ _ _ _ elems _) = findElemData elems
findElemData ∷ [WinElem] → (Maybe (WorldParams,WorldData))
findElemData []                            = Nothing
findElemData ((WinElemNULL):elems)         = findElemData elems
findElemData ((WinElemText _ _ _):elems)   = findElemData elems
findElemData ((WinElemTTF _ _ _ _):elems)    = findElemData elems
findElemData ((WinElemMenu _ _ _):elems)   = findElemData elems
findElemData ((WinElemDyn _ _):elems)      = findElemData elems
findElemData ((WinElemBack _):elems)       = findElemData elems
findElemData ((WinElemLink _ _ _):elems)   = findElemData elems
findElemData ((WinElemWorld wp wd _):_) = Just (wp,wd)

-- replaces world data in a window
replaceWorldData ∷ Window → WorldData → Window
replaceWorldData (Window name wType wArgV curs elems cache) wd = Window name wType wArgV curs (map (replaceElemData wd) elems) (reloadcache)
  where reloadcache = take (length cache) (repeat WEUncached)
replaceElemData ∷ WorldData → WinElem → WinElem
replaceElemData _   (WinElemNULL)            = WinElemNULL
replaceElemData _   (WinElemText tp tb ts)   = WinElemText tp tb ts
replaceElemData _   (WinElemTTF tp tz tb ts) = WinElemTTF tp tz tb ts
replaceElemData _   (WinElemMenu mn mp mb)   = WinElemMenu mn mp mb
replaceElemData wd' (WinElemWorld wp _ wt)   = WinElemWorld wp wd' wt
replaceElemData _   (WinElemLink lp lb la)   = WinElemLink lp lb la
replaceElemData _   (WinElemBack fp)         = WinElemBack fp
replaceElemData _   (WinElemDyn dt dd)       = WinElemDyn dt dd

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
          False → zones ⧺ [genNewZone zoneSize zoneInd segInd newSeg]--zones ⧺ [(Zone zoneInd [])]
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

-- if no zone exists we generate a new one
genNewZone ∷ (Int,Int) → (Int,Int) → (Int,Int) → Segment → Zone
genNewZone (zw,zh) zoneInd segInd newSeg = Zone zoneInd segs
  where segs' = take zh (repeat (take zw (repeat (SegmentNULL))))
        segs  = findAndReplaceSegmentSpot (zw,zh) segInd' segs' newSeg
        segInd' = offsetInd (zw,zh) segInd

-- change segInd when out of bounds
offsetInd ∷ (Int,Int) → (Int,Int) → (Int,Int)
offsetInd (zw,zh) (sx,sy) = (si,sj)
  where si = if (sx < 0) then (sx + zw)
             else if (sx > zw) then (sx - zw)
             else sx
        sj = if (sy < 0) then (sy + zh)
             else if (sy > zh) then (sy - zh)
             else sy

findAndReplaceWindow ∷ Window → [Window] → [Window]
findAndReplaceWindow _ [] = []
findAndReplaceWindow (Window testname a1 a2 a3 a4 a5) ((Window name b1 b2 b3 b4 b5):ws)
  | testname ≡ name = [Window testname a1 a2 a3 a4 a5] ⧺ findAndReplaceWindow (Window testname a1 a2 a3 a4 a5) ws
  | otherwise       = [Window name     b1 b2 b3 b4 b5] ⧺ findAndReplaceWindow (Window testname a1 a2 a3 a4 a5) ws
