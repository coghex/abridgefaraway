module Game.Util ( sceneSetup, Image(..), initTexs ) where

import Data.Bits ((.|.))
import Data.Word ( Word8, Word32 )
import Data.Serialize.Get ( Get, runGet, getWord32le, getWord16le
                          , skip, getByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign ( Ptr, pokeElemOff, peekElemOff, plusPtr, withForeignPtr, peek, alloca, free, malloc, ptrToIntPtr )
import Graphics.GL
import Control.Monad.RWS.Strict (put, liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLU
import Graphics.GLUtil
import qualified Codec.Picture as JT
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GL.Texturing
import System.Exit
import Game.Paths

--import Game.Font


data Image = Image !Int !Int !BS.ByteString

resizeScene :: GLFW.WindowSizeCallback
resizeScene win w     0      = resizeScene win w 1
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 500
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
  glFlush

initTexs :: GLFW.Window -> IO ([TextureObject])
initTexs win = do
  glEnable GL_TEXTURE_2D
  glShadeModel GL_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST
  (w, h) <- GLFW.getFramebufferSize win
  resizeScene win w h
  loadWTextures "data/maps/"

sceneSetup :: IO ()
sceneSetup = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT

loadZTextures :: String -> IO ([[TextureObject]])
loadZTextures fn = do
  --p1 <- loadGLZTextures (fn ++ "plains/plains001.bmp")
  --f1 <- loadGLZTextures (fn ++ "fields/fields001.bmp")
  --f2 <- loadGLZTextures (fn ++ "fields/fields002.bmp")
  --f3 <- loadGLZTextures (fn ++ "fields/fields003.bmp")
  --f4 <- loadGLZTextures (fn ++ "fields/fields004.bmp")
  --f5 <- loadGLZTextures (fn ++ "fields/fields005.bmp")
  --f6 <- loadGLZTextures (fn ++ "fields/fields006.bmp")
  --f7 <- loadGLZTextures (fn ++ "fields/fields007.bmp")
  --c1 <- loadGLZTextures (fn ++ "crags/crags001.bmp")
  --b1 <- loadGLZTextures (fn ++ "beach/beach001.bmp")
  --s1 <- loadGLZTextures (fn ++ "sea/sea001.bmp")
  --return ([[p1], [f1, f2, f3, f4, f5, f6, f7], [c1], [b1], [], [s1], [], []])
  return ([[], [], [], [], [], [], []])

loadWTextures :: String -> IO ([TextureObject])
loadWTextures fn = do
  t1 <- loadGLZTextures (fn ++ "plains/worldplains.png")
  t2 <- loadGLZTextures (fn ++ "fields/worldfields.png")
  t3 <- loadGLZTextures (fn ++ "crags/worldcrags.png")
  t4 <- loadGLZTextures (fn ++ "waste/worldwaste.png")
  t5 <- loadGLZTextures (fn ++ "sea/worldsea.png")
  t6 <- loadGLZTextures (fn ++ "ice/worldice.png")
  
  --return ([t1, t2, t3, t4, t5, t6, t7, t8], (zt!!0), zt)
  return ([t1, t2, t3, t4, t5, t6])

loadGLZTextures :: String -> IO GL.TextureObject
loadGLZTextures fn = do
  t <- either error id <$> readTexture fn
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
  return t


fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x


