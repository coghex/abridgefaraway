module GLUtil.UI where
-- the ui elements and how they are drawn are defined

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified GLUtil.ABFA as GLFW
import GLUtil.Font
import GLUtil.Util
import GLUtil.Draw
import ABFA.State
import ABFA.Settings
import ABFA.Game
import ABFA.Data


-- draws the menu
drawMenu :: State -> Env -> IO ()
drawMenu state env = do
  let settings = stateSettings  state
      w        = settingScreenW settings
      h        = settingScreenH settings
      rw       = settingRefSW   settings
      rh       = settingRefSH   settings
      fonts    = envFonts env
      dfont    = fonts !! 1
      cfont    = fonts !! 2
      fsize    = settingFontSize settings
      x        = -8
      y        = 5
  beginDrawFont
  drawFont cfont fsize FNULL   (x, y) "A Bridge Far Away..."
  drawFont dfont fsize FYELLOW (x, (y-3)) "C"
  drawFont dfont fsize FNULL   ((x+(lengthOfString dfont "C")), (y-3)) "reate New World"
  drawFont dfont fsize FYELLOW (x, (y-4)) "S"
  drawFont dfont fsize FNULL   ((x+(lengthOfString dfont "S")), (y-4)) "ettings"
  drawFont dfont fsize FYELLOW (x, (y-5)) "Esc"
  drawFont dfont fsize FNULL   ((x+(lengthOfString dfont "Esc")), (y-5)) "ape"

-- draws a standard loading screen
drawLoadScreen :: State -> Env -> String -> IO ()
drawLoadScreen state env string = do
  let settings = stateSettings state
      fonts    = envFonts env
      dfont    = fonts !! 1
      cfont    = fonts !! 2
      fsize    = settingFontSize settings
      x        = -8
      y        = 5
  beginDrawFont
  drawFont cfont fsize FNULL   (x, y) "Loading..."
  drawFont dfont fsize FNULL   (x, (y-3)) string

-- draws the UI for the world screen
drawWorldUI :: State -> Env -> IO ()
drawWorldUI state env = do
-- drawTopLeftText state env "test"
  return ()

-- draws a box that text looks good in
drawTextBox :: [GL.TextureObject] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
drawTextBox texs screenw screenh x y sx sy bx by = do
  GL.loadIdentity
  GL.translate $ GL.Vector3 nx ny nz
  where (nx, ny, nz) = worldZoom x y screenw screenh 60 30


-- this box will display world information
drawTopLeftText :: State -> Env -> String -> IO ()
drawTopLeftText state env str = do
  let settings = stateSettings state
      w  = settingScreenW settings
      h  = settingScreenH settings
      rw = settingRefSW   settings
      rh = settingRefSH   settings
      fonts = envFonts env
      dfont = fonts !! 1
      cfont = fonts !! 2
  beginDrawFont
  drawFont dfont 40 FNULL (0, 0) str