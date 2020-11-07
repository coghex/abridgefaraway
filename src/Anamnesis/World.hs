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
import Anamnesis.Draw
import Epiklesis.Data
import Epiklesis.World
import Paracletus.Data

createWorld ∷ Int → Int → Int → Int → String → World
createWorld sw sh zw zh texs = World [initZone] (sw,sh) texs
  where initZone = Zone (0,0) $ take zh (repeat (take zw (repeat (seg))))
        seg = SegmentNULL--Segment $ take sh $ repeat $ take sw $ repeat $ Tile 1 1

updateWorld ∷ Env → Int → ((Float,Float),(Int,Int)) → [((Int,Int),Segment)] → TState → IO ()
updateWorld env n sc segs TStop = do
  let scchan    = envSCChan env
      timerChan = envWTimerChan env
      eventQ = envEventsChan env
  tsnew ← atomically $ readChan timerChan
  firstSC ← atomically $ readChan scchan
  updateWorld env n sc segs tsnew
updateWorld env n sc segs TStart = do
  start ← getCurrentTime
  let scchan    = envSCChan env
      timerChan = envWTimerChan env
      segChan   = envSegChan env
      eventQ = envEventsChan env
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
updateWorld env n sc segs TPause = do
  let scchan = envSCChan env
  newSC ← atomically $ readChan scchan
  sendSegs env segs
  updateWorld env 0 newSC segs TStart
updateWorld _   _ _  _    TNULL = return ()

sendSegs ∷ Env → [((Int,Int),Segment)] → IO ()
sendSegs _   []          = return ()
sendSegs env ((sp,s):ss) = do
  let eventQ = envEventsChan env
  atomically $ writeQueue eventQ $ EventUpdateSegs (sp,s)
  sendSegs env ss

-- returns the list of indecies
-- of segments to generate
evalScreenCursor ∷ ((Float,Float),(Int,Int)) → [(Int,Int)]
evalScreenCursor ((cx,cy),(sw,wh)) = [(round cx,round cy)]

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
  atomically $ writeChan (envSCChan env) sc

-- converts elements in lua window to
-- text boxs in the actual draw state
calcTextBoxs ∷ Window → [TextBox]
calcTextBoxs win = luaTBtoWinTB (windowText win)
                 ⧺ luaMenutoWinTB (windowMenus win)

luaTBtoWinTB ∷ [WinText] → [TextBox]
luaTBtoWinTB []       = []
luaTBtoWinTB (wt:wts) = luaTBtoWinTB wts ⧺ [textBox]
  where textBox = TextBox { tbPos    = (tbx,tby)
                          , tbSize   = (3+tbsize,1)
                          , tbBox    = wb
                          , tbString = tbstr }
        (tbx, tby) = winPos wt
        (tbstr)    = winText wt
        tbsize     = round $ fromIntegral((length tbstr)) / (2.0 ∷ Double)
        wb         = winBox wt

luaMenutoWinTB ∷ [WinMenu] → [TextBox]
luaMenutoWinTB []       = []
luaMenutoWinTB (wm:wms) = luaMenutoWinTB wms ⧺ [textBox]
  where textBox = TextBox { tbPos = menuPos wm
                          , tbSize = (3+tbsize,tbheight)
                          , tbBox = True
                          , tbString = elemsToString (menuElems wm) }
        tbsize     = 20
        tbheight   = length $ menuElems wm

-- converts list of menu elements into
-- a string for a textbox
elemsToString ∷ [WinElem] → String
elemsToString []       = ""
elemsToString (we:wes) = (elemToString we) ⧺ "\n" ⧺ (elemsToString wes)

elemToString ∷ WinElem → String
elemToString (WinElemText text) = text
elemToString (WinElemSlider x y d args) = args ⧺ (show d) ⧺ "   " ⧺ (show x) ⧺ " <--|--> " ⧺ (show y)
elemToString WinElemNULL = "NULL"

-- converts tiles from a window into GTiles
calcTiles ∷ Window → [GTile]
calcTiles win = luaTiletoWinTile 0 $ windowTiles win

flatten ∷ [[α]] → [α]
flatten xs = (\z n → foldr (\x y → foldr z y x) n xs) (:) []

-- converts tiles from the world object into GTiles
calcWorldTiles ∷ ((Float,Float),(Int,Int)) → Window → Int → [GTile]
calcWorldTiles sc (Window _ _ _ _ _ _ _ WorldNULL) _ = []
calcWorldTiles sc (Window _ _ _ _ _ _ _ (World zones segsize texs)) nModTiles = calcZoneTiles sc segsize zones nModTiles
calcZoneTiles ∷ ((Float,Float),(Int,Int)) → (Int,Int) → [Zone] → Int → [GTile]
calcZoneTiles sc segsize []           nModTiles = []
calcZoneTiles sc segsize (ZoneNULL:zones) nModTiles = calcZoneTiles sc segsize zones nModTiles
calcZoneTiles sc segsize (zone:zones) nModTiles = (calcSegTiles sc segsize zone nModTiles) ⧺ (calcZoneTiles sc segsize zones nModTiles)
calcSegTiles ∷ ((Float,Float),(Int,Int)) → (Int,Int) → Zone → Int → [GTile]
calcSegTiles sc segsize (Zone ind segs) nModTiles = flatten $ map (calcSegTilesRow sc segsize ind nModTiles) (zip yinds segs)
  where yinds = take (fst segsize) [0..]
calcSegTilesRow ∷ ((Float,Float),(Int,Int)) → (Int,Int) → (Int,Int) → Int → (Int,[Segment]) → [GTile]
calcSegTilesRow sc segsize ind nModTiles (j,segs) = flatten $ map (calcSegTilesSpot j sc segsize ind nModTiles) (zip xinds segs)
  where xinds = take (snd segsize) [0..]
calcSegTilesSpot ∷ Int → ((Float,Float),(Int,Int)) → (Int,Int) → (Int,Int) → Int → (Int,Segment) → [GTile]
calcSegTilesSpot j sc segsize ind nModTiles (i,seg) = calcGTileFromSeg i j sc segsize ind seg nModTiles

calcGTileFromSeg ∷ Int → Int → ((Float,Float),(Int,Int)) → (Int,Int) → (Int,Int) → Segment → Int → [GTile]
calcGTileFromSeg m n sc (sw,sh) ind (SegmentNULL) nModTiles = []
calcGTileFromSeg m n sc (sw,sh) ind (Segment grid) nModTiles = tiles
  where tiles = flatten $ calcWorldTilesRow sc' nModTiles (sw*(m + (fst ind)),sh*(n + (snd ind))) grid
        sc'   = roundsc sc
        roundsc ∷ ((Float,Float),(Int,Int)) → (Int,Int,Int,Int)
        roundsc ((x,y),(w,h)) = (round x,round y,w,h)

calcWorldTilesRow ∷ (Int,Int,Int,Int) → Int → (Int,Int) → [[Tile]] → [[GTile]]
calcWorldTilesRow _             _         _     []           = [[]]
calcWorldTilesRow _             _         _     [[]]         = [[]]
calcWorldTilesRow (cx,cy,cw,ch) nModTiles (x,y) (grow:grows)
  | ((y > (cy+ch)) ∨ (y < (cy-ch))) = [[]] ⧺ (calcWorldTilesRow (cx,cy,cw,ch) nModTiles (x,(y+1)) grows)
  | otherwise = [(calcWorldTilesSpot (cx,cy,cw,ch) nModTiles (x,y) grow)] ⧺ (calcWorldTilesRow (cx,cy,cw,ch) nModTiles (x,(y+1)) grows)

calcWorldTilesSpot ∷ (Int,Int,Int,Int) → Int → (Int,Int) → [Tile] → [GTile]
calcWorldTilesSpot _             _         _     []             = []
calcWorldTilesSpot (cx,cy,cw,ch) nModTiles (x,y) (gspot:gspots)
  | ((x > (cx+cw)) ∨ (x < (cx-cw))) = [] ⧺ (calcWorldTilesSpot (cx,cy,cw,ch) nModTiles ((x+1),y) gspots)
  | otherwise = [tile] ⧺ (calcWorldTilesSpot (cx,cy,cw,ch) nModTiles ((x+1),y) gspots)
  where tile = GTile { tPos = (((fromIntegral x) - 1.0), ((fromIntegral y) - 1.0))
                     , tScale = (1,1)
                     , tInd = (ix,iy)
                     , tSize = (3,15)
                     , tT = (20+nModTiles+(tileCont gspot))
                     , tMoves = True }
        ix = (tileType gspot) `mod` 3
        iy = (tileType gspot) `div` 3

-- segment index helper functions
getSegment ∷ (Int,Int) → [[Segment]] → Segment
getSegment pos segs = findSegRow pos 0 segs
findSegRow ∷ (Int,Int) → Int → [[Segment]] → Segment
findSegRow _ _ [[]] = SegmentNULL
findSegRow _ _ []   = SegmentNULL
findSegRow (i,j) n (segrow:segrows)
  | (n == j)  = findSegSpot i 0 segrow
  | otherwise = findSegRow (i,j) (n+1) segrows
findSegSpot ∷ Int → Int → [Segment] → Segment
findSegSpot _ _ []         = SegmentNULL
findSegSpot i n (seg:segs)
  | (n == i)  = seg
  | otherwise = findSegSpot i (n+1) segs
findAndReplaceSegment ∷ ((Int,Int),Segment) → [[Segment]] → [[Segment]]
findAndReplaceSegment ((sx,sy),seg) segs = addToSegRow 0 sx sy seg segs
addToSegRow ∷ Int → Int → Int → Segment → [[Segment]] → [[Segment]]
addToSegRow _ _  _  _      [[]]             = [[]]
addToSegRow _ _  _  _      []               = []
addToSegRow n sx sy newseg (segrow:segrows) = [thissegrow] ⧺ addToSegRow (n+1) sx sy newseg segrows
  where thissegrow = addToSegSpot sx sy 0 n newseg segrow
addToSegSpot ∷ Int → Int → Int → Int → Segment → [Segment] → [Segment]
addToSegSpot _  _  _ _ _      []         = []
addToSegSpot sx sy m n newseg (seg:segs)
  | ((sx == m) ∧ (sy == n)) = [newseg] ⧺ addToSegSpot sx sy (m+1) n newseg segs
  | otherwise               = [seg] ⧺ addToSegSpot sx sy (m+1) n newseg segs
replaceWindow ∷ Window → [Window] → [Window]
replaceWindow _      []         = []
replaceWindow newwin (win:wins)
  | (winTitle newwin) == (winTitle win) = [newwin] ⧺ replaceWindow newwin wins
  | otherwise = [win] ⧺ replaceWindow newwin wins

luaTiletoWinTile ∷ Int → [WinTile] → [GTile]
luaTiletoWinTile _ []       = []
luaTiletoWinTile n (wt:wts) = (luaTiletoWinTile (n+1) wts) ⧺ [tile]
  where tile = GTile { tPos = winTilePos wt
                     , tScale = (1,1)
                     , tInd = (0,0)
                     , tSize = (1,1)
                     , tT = (20+n)
                     , tMoves = True }
