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
  where seg = Segment $ take 32 (repeat (take 32 (repeat (Tile 2 1))))

-- sends the updating thread the screen cursor
reloadScreenCursor ∷ Env → ((Float,Float),(Int,Int)) → IO ()
reloadScreenCursor env sc = do
  atomically $ writeChan (envWTimerChan env) TPause
  atomically $ writeChan (envCamChan env) sc
