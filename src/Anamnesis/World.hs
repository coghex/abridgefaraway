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
createWorld sw sh zw zh texs = World [initZone] texs
  where initZone = Zone (0,0) $ initSegments zw zh
        initSegments ∷ Int → Int → [[Segment]]
        initSegments w h = make0Seg $ take h (repeat (take w (repeat (SegmentNULL))))
        make0Seg ∷ [[Segment]] → [[Segment]]
        make0Seg segs = [make0SegRow] ⧺ (tail segs)
          where make0SegRow = [newseg] ⧺ (tail $ head segs)
                newseg = Segment $ take sh $ repeat $ take sw $ repeat $ Tile 1 1

updateWorld ∷ Env → ((Float,Float),(Int,Int)) → TState → IO ()
updateWorld env sc TStop = do
  let scchan    = envSCChan env
      timerChan = envWTimerChan env
  tsnew ← atomically $ readChan timerChan
  firstSC ← atomically $ readChan scchan
  updateWorld env sc tsnew
updateWorld env sc TStart = do
  start ← getCurrentTime
  let scchan    = envSCChan env
      timerChan = envWTimerChan env
  timerstate <- atomically $ tryReadChan timerChan
  tsnew <- case (timerstate) of
    Nothing -> return TStart
    Just x  -> return x
  -- logic goes here
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) :: Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  updateWorld env sc tsnew
updateWorld env sc TPause = do
  let scchan = envSCChan env
      eventQ = envEventsChan env
  newSC ← atomically $ readChan scchan
  atomically $ writeQueue eventQ $ EventLogDebug $ "screenCursor updated to: " ⧺ (show newSC)
  updateWorld env newSC TStart
updateWorld _   _  TNULL = return ()

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
calcWorldTiles sc (Window _ _ _ _ _ _ _ (World zones texs)) nModTiles = calcZoneTiles sc zones nModTiles
calcZoneTiles ∷ ((Float,Float),(Int,Int)) → [Zone] → Int → [GTile]
calcZoneTiles _  []                      _         = []
calcZoneTiles sc (ZoneNULL:zones)        nModTiles = calcZoneTiles sc zones nModTiles
calcZoneTiles sc ((Zone ind segs):zones) nModTiles = (calcSegTiles sc ind segs nModTiles) ⧺ (calcZoneTiles sc zones nModTiles)
calcSegTiles ∷ ((Float,Float),(Int,Int)) → (Int,Int) → [[Segment]] → Int → [GTile]
calcSegTiles _  _   []               _         = []
calcSegTiles _  _   [[]]             _         = []
calcSegTiles sc ind (segrow:segrows) nModTiles = (calcSegTilesRow sc ind segrow nModTiles) ⧺ (calcSegTiles sc ind segrows nModTiles)
calcSegTilesRow ∷ ((Float,Float),(Int,Int)) → (Int,Int) → [Segment] → Int → [GTile]
calcSegTilesRow _  _   []         _         = []
calcSegTilesRow sc ind (seg:segs) nModTiles = (calcSegTilesSpot sc ind seg nModTiles) ⧺ calcSegTilesRow sc ind segs nModTiles
calcSegTilesSpot ∷ ((Float,Float),(Int,Int)) → (Int,Int) → Segment → Int → [GTile]
calcSegTilesSpot _  _   SegmentNULL    _         = []
calcSegTilesSpot sc ind (Segment grid) nModTiles = tiles
  where tiles = flatten $ calcWorldTilesRow sc' nModTiles (0,0) grid
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

-- segment index helper function
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

luaTiletoWinTile ∷ Int → [WinTile] → [GTile]
luaTiletoWinTile _ []       = []
luaTiletoWinTile n (wt:wts) = (luaTiletoWinTile (n+1) wts) ⧺ [tile]
  where tile = GTile { tPos = winTilePos wt
                     , tScale = (1,1)
                     , tInd = (0,0)
                     , tSize = (1,1)
                     , tT = (20+n)
                     , tMoves = True }
