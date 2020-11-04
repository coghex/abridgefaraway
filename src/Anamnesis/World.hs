module Anamnesis.World where
-- various functions to make lua world
-- gen faster
import Prelude()
import UPrelude
import Anamnesis.Draw
import Epiklesis.Data
import Epiklesis.World
import Paracletus.Data

createWorld ∷ Int → Int → [[Int]]
createWorld w h = take h (repeat (take w (repeat 1)))

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
calcWorldTiles ∷ (Float,Float,Int,Int) → Window → Int → [GTile]
calcWorldTiles sc (Window _ _ _ _ _ _ _ WorldNULL) _ = []
calcWorldTiles sc (Window _ _ _ _ _ _ _ (World size grid texs)) nModTiles = tiles
  where tiles = flatten $ calcWorldTilesRow sc' nModTiles (0,0) grid
        sc'   = roundsc sc
        roundsc ∷ (Float,Float,Int,Int) → (Int,Int,Int,Int)
        roundsc (x,y,w,h) = (round x,round y,w,h)

calcWorldTilesRow ∷ (Int,Int,Int,Int) → Int → (Int,Int) → [[Int]] → [[GTile]]
calcWorldTilesRow _             _         _     []           = [[]]
calcWorldTilesRow _             _         _     [[]]         = [[]]
calcWorldTilesRow (cx,cy,cw,ch) nModTiles (x,y) (grow:grows)
  | ((y > (cy+ch)) ∨ (y < (cy-ch))) = [[]] ⧺ (calcWorldTilesRow (cx,cy,cw,ch) nModTiles (x,(y+1)) grows)
  | otherwise = [(calcWorldTilesSpot (cx,cy,cw,ch) nModTiles (x,y) grow)] ⧺ (calcWorldTilesRow (cx,cy,cw,ch) nModTiles (x,(y+1)) grows)

calcWorldTilesSpot ∷ (Int,Int,Int,Int) → Int → (Int,Int) → [Int] → [GTile]
calcWorldTilesSpot _             _         _     []             = []
calcWorldTilesSpot (cx,cy,cw,ch) nModTiles (x,y) (gspot:gspots)
  | ((x > (cx+cw)) ∨ (x < (cx-cw))) = [] ⧺ (calcWorldTilesSpot (cx,cy,cw,ch) nModTiles ((x+1),y) gspots)
  | otherwise = [tile] ⧺ (calcWorldTilesSpot (cx,cy,cw,ch) nModTiles ((x+1),y) gspots)
  where tile = GTile { tPos = (((fromIntegral x) - 1.0), ((fromIntegral y) - 1.0))
                     , tScale = (1,1)
                     , tInd = (gspot,0)
                     , tSize = (3,15)
                     , tT = (20+nModTiles)
                     , tMoves = True }

luaTiletoWinTile ∷ Int → [WinTile] → [GTile]
luaTiletoWinTile _ []       = []
luaTiletoWinTile n (wt:wts) = (luaTiletoWinTile (n+1) wts) ⧺ [tile]
  where tile = GTile { tPos = winTilePos wt
                     , tScale = (1,1)
                     , tInd = (0,0)
                     , tSize = (1,1)
                     , tT = (20+n)
                     , tMoves = True }
