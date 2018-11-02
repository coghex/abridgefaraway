module Game.Font where

import System.Directory ( doesFileExist )
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
import Game.Data
import Font.Rendering.Text.Renderer
import Font.Rendering.Text.Types
import Font.Rendering.Shader.Shape as S
import Font.Rendering.Shader.Text as T
import Font.Cacheing.Text
import Font.Math
import Font.Utils
import Font.Texture.Load
import Font.Editor.Types


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

-- these are the new font functions
initFontRenderer :: String -> IO TRend
initFontRenderer s = do
  exists <- doesFileExist s
  unless exists $ fail $ s ++ " does not exist"
  let load tr = loadCharMap tr "hmmm..."
  texRenderer <- makeTextRenderer s 16 >>= load
  shapeShader <- makeShapeShaderProgram
  return $ TRend texRenderer (0,0) shapeShader Nothing
  
