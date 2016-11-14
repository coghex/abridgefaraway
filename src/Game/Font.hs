module Game.Font where

import Control.Monad
import Graphics.Rendering.OpenGL hiding (bitmap)
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

beginDrawText :: IO ()
beginDrawText = do
  sceneSetup
  blend                      $= Enabled
  blendFunc                  $= (SrcAlpha, OneMinusSrcAlpha)

loadFont :: String -> IO (FTGL.Font)
loadFont fn = FTGL.createTextureFont "data/fonts/amatic/AmaticSC-Regular.ttf"

drawText :: FTGL.Font -> Int -> Int -> Int -> Int -> String -> IO ()
drawText font x y sx sy str = do
  loadIdentity
  translate $ Vector3 (2*((fromIntegral x) - (fromIntegral 60))) (2*((fromIntegral y) - (fromIntegral 45))) (-400::GLfloat)
  color (Color3 1 1 1 :: Color3 GLfloat)
  -- you need to draw a square here
  --font <- FTGL.createTextureFont "data/fonts/amatic/AmaticSC-Regular.ttf"
  FTGL.setFontFaceSize font sx sy
  FTGL.renderFont font str FTGL.Front
  


