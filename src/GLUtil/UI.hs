module GLUtil.UI where
-- the ui elements and how they are drawn are defined

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified GLUtil.ABFA as GLFW
import GLUtil.Font
import GLUtil.Util
import GLUtil.Draw
import GLUtil.Textures
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
  glClear $ fromIntegral $ GL_DEPTH_BUFFER_BIT
  GL.translate $ GL.Vector3 (fromIntegral (2*x)) (fromIntegral (2*y)) (-64::GL.GLfloat)
  -- first render top left corner
  withTextures2D [texs!!7] $ drawBoxTile
  -- render the top row
  drawTextBoxTopRow texs bx
  -- render top right corner
  GL.translate $ GL.Vector3 (2.0::GL.GLfloat) (0.0::GL.GLfloat) (0.0::GL.GLfloat)
  withTextures2D [texs!!5] $ drawBoxTile
  -- render the middle rows
  drawTextBoxMiddleRows texs bx by
  -- render the bottom right corner
  GL.translate $ GL.Vector3 ((fromIntegral(-2*(bx+1)))::GL.GLfloat) (-2.0::GL.GLfloat) (0.0::GL.GLfloat)
  withTextures2D [texs!!1] $ drawBoxTile
  -- render the bottom row
  drawTextBoxBottomRow texs bx
  -- render the bottom right corner
  GL.translate $ GL.Vector3 (2.0::GL.GLfloat) (0.0::GL.GLfloat) (0.0::GL.GLfloat)
  withTextures2D [texs!!3] $ drawBoxTile

-- draws the top row
drawTextBoxTopRow :: [GL.TextureObject] -> Int -> IO ()
drawTextBoxTopRow texs 0 = return ()
drawTextBoxTopRow texs n = do
  GL.translate $ GL.Vector3 (2.0::GL.GLfloat) (0.0::GL.GLfloat) (0.0::GL.GLfloat)
  withTextures2D [texs!!6] $ drawBoxTile
  drawTextBoxTopRow texs (n-1)

-- draws the middle rows
drawTextBoxMiddleRows :: [GL.TextureObject] -> Int -> Int -> IO ()
drawTextBoxMiddleRows texs bx 0 = return ()
drawTextBoxMiddleRows texs bx n = do
  -- render first tile
  GL.translate $ GL.Vector3 ((fromIntegral(-2*(bx+1)))::GL.GLfloat) (-2.0::GL.GLfloat) (0.0::GL.GLfloat)
  withTextures2D [texs!!8] $ drawBoxTile
  -- render the middle tiles
  drawTextBoxMiddleBit texs bx
  -- render the last tile
  GL.translate $ GL.Vector3 (2.0::GL.GLfloat) (0.0::GL.GLfloat) (0.0::GL.GLfloat)
  withTextures2D [texs!!4] $ drawBoxTile
  drawTextBoxMiddleRows texs bx (n-1)

-- draws the bit in the middle
drawTextBoxMiddleBit :: [GL.TextureObject] -> Int -> IO ()
drawTextBoxMiddleBit texs 0 = return ()
drawTextBoxMiddleBit texs n = do
  GL.translate $ GL.Vector3 (2.0::GL.GLfloat) (0.0::GL.GLfloat) (0.0::GL.GLfloat)
  withTextures2D [texs!!0] $ drawBoxTile
  drawTextBoxMiddleBit texs (n-1)

-- draws the bottom row
drawTextBoxBottomRow :: [GL.TextureObject] -> Int -> IO ()
drawTextBoxBottomRow texs 0 = return ()
drawTextBoxBottomRow texs n = do
  GL.translate $ GL.Vector3 (2.0::GL.GLfloat) (0.0::GL.GLfloat) (0.0::GL.GLfloat)
  withTextures2D [texs!!2] $ drawBoxTile
  drawTextBoxBottomRow texs (n-1)

-- draws the actual box tiles
drawBoxTile :: IO ()
drawBoxTile = do
  glBegin GL_QUADS
  glTexCoord2f   0    0
  glVertex3f   (-1) (-1)  1
  glTexCoord2f   1    0
  glVertex3f     1  (-1)  1
  glTexCoord2f   1    1
  glVertex3f     1    1   1
  glTexCoord2f   0    1
  glVertex3f   (-1)   1   1
  glEnd

-- this element will display world information
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
