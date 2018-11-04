module Game.Font where

import Control.Monad
import Data.Maybe
import qualified Graphics.GL as GLR
import Graphics.Rendering.OpenGL hiding (bitmap,Matrix)
import Graphics.Rendering.FreeType.Internal
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize as BS
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.FaceType
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Foreign
import Foreign.C.String
import Graphics.Rendering.FreeType.Internal.Bitmap
import qualified Graphics.Rendering.FTGL as FTGL
import Game.Settings
import Game.Util
import Game.Data
import Game.Map
import GLUtil.Textures


beginDrawText :: IO ()
beginDrawText = do
  sceneSetup
  blend                      $= Enabled
  blendFunc                  $= (SrcAlpha, OneMinusSrcAlpha)

loadFont :: String -> IO (FTGL.Font)
loadFont fn = FTGL.createTextureFont fn

drawText :: FTGL.Font -> Int -> Int -> Int -> Int -> String -> IO ()
drawText font x y sx sy str = do
  loadIdentity
  translate $ Vector3 (2*((fromIntegral x) - (fromIntegral 60))) (2*((fromIntegral y) - (fromIntegral 45))) (-500::GLfloat)
  color (Color3 1 1 1 :: Color3 GLfloat)
  FTGL.setFontFaceSize font (quot sx 2) (quot sy 2)
  FTGL.renderFont font str FTGL.Front

drawZoneText :: [[TextureObject]] -> FTGL.Font -> Int -> Int -> Int -> Int -> [String] -> IO ()
drawZoneText texs font x y sx sy str = do
  drawTextBox texs' x y sx sy (quot (maximum (map length str)) 2) (length str)
  loadIdentity
  translate $ Vector3 ((2*((fromIntegral x)-(fromIntegral 60)))/100) ((2*((fromIntegral y)-(fromIntegral 45)))/100) (-5::GLfloat)
  GLR.glScalef 0.01 0.01 0.01
  color (Color3 1 1 1 :: Color3 GLfloat)
  FTGL.setFontFaceSize font (quot sx 2) (quot sy 2)
  resequence_ $ map (renderFontMap sy font) str
  where texs' = texs !! 1

renderFontMap :: Int -> FTGL.Font -> String -> IO ()
renderFontMap size font str = do
  FTGL.renderFont font str FTGL.Front
  translate $ Vector3 (0.0::GLfloat) ((16.0-size')::GLfloat) (0.0::GLfloat)
  where size' = fromIntegral size

drawTextBox :: [TextureObject] -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
drawTextBox texs x y sx sy bx by = do
  loadIdentity
  translate $ Vector3 ((2*((fromIntegral x)-(fromIntegral 60)))/50) ((2*((fromIntegral y)-(fromIntegral 32)))/50) (-10::GLfloat)
  GLR.glScalef 0.2 0.2 0.2
  -- first render top left corner
  withTextures2D [texs!!7] $ drawBoxTile texs
  -- render the top row
  drawTextBoxTopRow texs bx
  -- render the top right corner
  translate $ Vector3 (2.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
  withTextures2D [texs!!5] $ drawBoxTile texs
  -- render the middlerows
  drawTextBoxMiddleRows texs bx by
  -- render the bottom right corner
  translate $ Vector3 ((fromIntegral(-2*(bx+1)))::GLfloat) (-2.0::GLfloat) (0.0::GLfloat)
  withTextures2D [texs!!1] $ drawBoxTile texs
  -- render the bottom row
  drawTextBoxBottomRow texs bx
  -- render the bottom right corner
  translate $ Vector3 (2.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
  withTextures2D [texs!!3] $ drawBoxTile texs


drawTextBoxMiddleRows :: [TextureObject] -> Int -> Int -> IO ()
drawTextBoxMiddleRows texs bx 0 = return ()
drawTextBoxMiddleRows texs bx n = do
  -- render first tile
  translate $ Vector3 ((fromIntegral(-2*(bx+1)))::GLfloat) (-2.0::GLfloat) (0.0::GLfloat)
  withTextures2D [texs!!8] $ drawBoxTile texs
  -- render the middle tiles
  drawTextBoxMiddleBit texs bx
  -- render the last tile
  translate $ Vector3 (2.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
  withTextures2D [texs!!4] $ drawBoxTile texs
  drawTextBoxMiddleRows texs bx (n-1)

drawTextBoxMiddleBit :: [TextureObject] -> Int -> IO ()
drawTextBoxMiddleBit texs 0 = return ()
drawTextBoxMiddleBit texs n = do
  translate $ Vector3 (2.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
  withTextures2D [texs!!0] $ drawBoxTile texs
  drawTextBoxMiddleBit texs (n-1)

drawTextBoxTopRow :: [TextureObject] -> Int -> IO ()
drawTextBoxTopRow texs 0 = return ()
drawTextBoxTopRow texs n = do
  translate $ Vector3 (2.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
  withTextures2D [texs!!6] $ drawBoxTile texs
  drawTextBoxTopRow texs (n-1)

drawTextBoxBottomRow :: [TextureObject] -> Int -> IO ()
drawTextBoxBottomRow texs 0 = return ()
drawTextBoxBottomRow texs n = do
  translate $ Vector3 (2.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
  withTextures2D [texs!!2] $ drawBoxTile texs
  drawTextBoxBottomRow texs (n-1)

drawBoxTile :: [TextureObject] -> IO ()
drawBoxTile tex = do
  GLR.glBegin GLR.GL_QUADS
  GLR.glTexCoord2f   0    0
  GLR.glVertex3f   (-1) (-1)  1
  GLR.glTexCoord2f   1    0
  GLR.glVertex3f     1  (-1)  1
  GLR.glTexCoord2f   1    1
  GLR.glVertex3f     1    1   1
  GLR.glTexCoord2f   0    1
  GLR.glVertex3f   (-1)   1   1
  GLR.glEnd








