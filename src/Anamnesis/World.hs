{-# LANGUAGE Strict #-}
module Anamnesis.World where
-- various functions to make lua world
-- gen faster
import Prelude()
import UPrelude
import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Artos.Data
import Artos.Var
import Artos.Queue
import Anamnesis.Data
import Epiklesis.Data
import Epiklesis.World
import Paracletus.Data

createWorld ∷ Int → Int → Int → Int → String → World
createWorld sw sh zw zh texs = World [initZone] (sw,sh) texs
  where initZone = Zone (0,0) $ take zh (repeat (take zw (repeat (seg))))
        seg = SegmentNULL--Segment $ take sh $ repeat $ take sw $ repeat $ Tile 1 1

updateWorld ∷ Env → Int → ((Float,Float),(Int,Int)) → [((Int,Int),Segment)] → TState → IO ()
updateWorld env n _  segs TStop = do
  let scchan    = envCamChan env
      timerChan = envWTimerChan env
  tsnew ← atomically $ readChan timerChan
  firstSC ← atomically $ readChan scchan
  updateWorld env n firstSC segs tsnew
updateWorld env n sc _    TStart = do
  start ← getCurrentTime
  let timerChan = envWTimerChan env
  timerstate <- atomically $ tryReadChan timerChan
  tsnew <- case (timerstate) of
    Nothing -> return TStart
    Just x  -> return x
  -- logic goes here
  let newsegs = genSegs $ evalScreenCursor sc
  newn ← if (n > 100)
         then do
           sendSegs env newsegs
           return 0
         else return (n+1)
  -- logic ends here
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) :: Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  updateWorld env newn sc newsegs tsnew
updateWorld env _ _  segs TPause = do
  let scchan = envCamChan env
  newSC ← atomically $ readChan scchan
  sendSegs env segs
  updateWorld env 0 newSC segs TStart
updateWorld _   _ _  _    TNULL = return ()

sendSegs ∷ Env → [((Int,Int),Segment)] → IO ()
sendSegs _   []          = return ()
sendSegs env ((sp,s):ss) = do
  let eventQ = envEventsChan env
  atomically $ writeQueue eventQ $ EventUpdateSegs $ SegUpdateData SegOpAdd (0,0) sp s
  sendSegs env ss

-- returns the list of indecies
-- of segments to generate
evalScreenCursor ∷ ((Float,Float),(Int,Int)) → [(Int,Int)]
evalScreenCursor ((cx,cy),_) = [pos]
  where pos = (x,y)
        x = floor $ cx / 32
        y = floor $ cy / 32

-- generates the segments that are
-- required by evalScreenCursor
genSegs ∷ [(Int,Int)] → [((Int,Int),Segment)]
genSegs []             = []
genSegs (pos:poss) = [(pos,seg)] ⧺ (genSegs poss)
  where seg = Segment $ take 8 (repeat (take 8 (repeat (Tile 2 1))))

-- sends the updating thread the screen cursor
reloadScreenCursor ∷ Env → ((Float,Float),(Int,Int)) → IO ()
reloadScreenCursor env sc = do
  atomically $ writeChan (envWTimerChan env) TPause
  atomically $ writeChan (envCamChan env) sc

-- returns the first world element
-- found on the current window
findWorldData ∷ Window → (Maybe (WorldParams,WorldData))
findWorldData (Window _ _ _ elems) = findElemData elems
findElemData ∷ [WinElem] → (Maybe (WorldParams,WorldData))
findElemData []                            = Nothing
findElemData ((WinElemNULL):elems)         = findElemData elems
findElemData ((WinElemText _ _ _):elems)   = findElemData elems
findElemData ((WinElemBack _):elems)       = findElemData elems
findElemData ((WinElemLink _ _ _):elems)   = findElemData elems
findElemData ((WinElemWorld wp wd _):elems) = Just (wp,wd)

-- replaces world data in a window
replaceWorldData ∷ Window → WorldData → Window
replaceWorldData (Window name wType curs elems) wd = Window name wType curs $ map (replaceElemData wd) elems
replaceElemData ∷ WorldData → WinElem → WinElem
replaceElemData wd' (WinElemNULL)           = WinElemNULL
replaceElemData wd' (WinElemText tp tb ts)  = WinElemText tp tb ts
replaceElemData wd' (WinElemWorld wp wd wt) = WinElemWorld wp wd' wt
replaceElemData wd' (WinElemLink lp lb la)  = WinElemLink lp lb la
replaceElemData wd' (WinElemBack fp)        = WinElemBack fp

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

-- tells us if a zone is loaded in
zoneExists ∷ (Int,Int) → [Zone] → Bool
zoneExists _       [] = False
zoneExists testInd ((Zone ind segs):zs)
  | (testInd ≡ ind) = True
  | otherwise       = zoneExists testInd zs

findAndReplaceWindow ∷ Window → [Window] → [Window]
findAndReplaceWindow (Window testname a1 a2 a3) ((Window name b1 b2 b3):ws)
  | testname ≡ name = [Window testname a1 a2 a3] ⧺ findAndReplaceWindow (Window testname a1 a2 a3) ws
  | otherwise       = [Window name     b1 b2 b3] ⧺ findAndReplaceWindow (Window testname a1 a2 a3) ws
