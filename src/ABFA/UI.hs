module ABFA.UI where
-- the ui elements and how they are drawn are defined

import GLUtil.Font
import ABFA.State
import ABFA.Settings
import ABFA.Game


-- draws the menu
drawMenu :: State -> Env -> IO ()
drawMenu state env = do
  let settings = stateSettings state
      w        = screenw settings
      h        = screenh settings
      rw       = refsw   settings
      rh       = refsh   settings
      fonts    = envFonts env
      dfont    = fonts !! 1
      cfont    = fonts !! 2
      x        = -8
      y        = 5
      fsize    = fontsize settings
  beginDrawFont
  drawFont cfont fsize FNULL   (x, y) "A Bridge Far Away..."
  drawFont dfont fsize FYELLOW (x, (y-4)) "C"
  drawFont dfont fsize FNULL   ((x+(lengthOfString dfont "C")), (y-4)) "reate New World"
  drawFont dfont fsize FYELLOW (x, (y-5)) "Esc"
  drawFont dfont fsize FNULL   ((x+(lengthOfString dfont "Esc")), (y-5)) "ape"

-- this box will display world information
drawTopLeftText :: State -> Env -> String -> IO ()
drawTopLeftText state env str = do
  let settings = stateSettings state
      w  = screenw settings
      h  = screenh settings
      rw = refsw   settings
      rh = refsh   settings
      fonts = envFonts env
      dfont = fonts !! 1
      cfont = fonts !! 2
  beginDrawFont
  drawFont dfont 40 FNULL (0, 0) str

