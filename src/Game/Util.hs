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

--import Game.Font


data Image = Image !Int !Int !BS.ByteString

sceneSetup :: IO ()
sceneSetup = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT

resizeScene :: GLFW.WindowSizeCallback
resizeScene win w     0      = resizeScene win w 1
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 500
  gluLookAt 0.0 0.0 0.0 0.0 0.0 (-1.0) 0.0 1.0 0.0
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
  glFlush

initTexs :: GLFW.Window -> IO ([TextureObject], [[TextureObject]])
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
  wtex <- loadWTextures "data/biome/"
  ztex <- loadZTextures "data/zone/"
  return (wtex, ztex)

loadZTextures :: String -> IO ([[TextureObject]])
loadZTextures fn = do
  p0 <- loadTex (fn ++ "plains/plains0.png")
  p1 <- loadTex (fn ++ "plains/plains1.png")
  p2 <- loadTex (fn ++ "plains/plains2.png")
  p3 <- loadTex (fn ++ "plains/plains3.png")
  p4 <- loadTex (fn ++ "plains/plains4.png")
  p5 <- loadTex (fn ++ "plains/plains5.png")
  p6 <- loadTex (fn ++ "plains/plains6.png")
  p7 <- loadTex (fn ++ "plains/plains7.png")
  p8 <- loadTex (fn ++ "plains/plains8.png")
  p9 <- loadTex (fn ++ "plains/plains9.png")
  p10 <- loadTex (fn ++ "plains/plains10.png")
  p11 <- loadTex (fn ++ "plains/plains11.png")
  p12 <- loadTex (fn ++ "plains/plains12.png")
  p13 <- loadTex (fn ++ "plains/plains13.png")
  p14 <- loadTex (fn ++ "plains/plains14.png")
  p15 <- loadTex (fn ++ "plains/plains15.png")
  p16 <- loadTex (fn ++ "plains/plains16.png")
  p17 <- loadTex (fn ++ "plains/plains17.png")
  p18 <- loadTex (fn ++ "plains/plains18.png")
  p19 <- loadTex (fn ++ "plains/plains19.png")
  p20 <- loadTex (fn ++ "plains/plains20.png")
  p21 <- loadTex (fn ++ "plains/plains21.png")
  p22 <- loadTex (fn ++ "plains/plains22.png")
  p23 <- loadTex (fn ++ "plains/plains23.png")
  p24 <- loadTex (fn ++ "plains/plains24.png")
  p25 <- loadTex (fn ++ "plains/plains25.png")
  p26 <- loadTex (fn ++ "plains/plains26.png")
  f0 <- loadTex (fn ++ "fields/fields0.png")
  f1 <- loadTex (fn ++ "fields/fields1.png")
  f2 <- loadTex (fn ++ "fields/fields2.png")
  f3 <- loadTex (fn ++ "fields/fields3.png")
  f4 <- loadTex (fn ++ "fields/fields4.png")
  f5 <- loadTex (fn ++ "fields/fields5.png")
  f6 <- loadTex (fn ++ "fields/fields6.png")
  f7 <- loadTex (fn ++ "fields/fields7.png")
  f8 <- loadTex (fn ++ "fields/fields8.png")
  f9 <- loadTex (fn ++ "fields/fields9.png")
  f10 <- loadTex (fn ++ "fields/fields10.png")
  f11 <- loadTex (fn ++ "fields/fields11.png")
  f12 <- loadTex (fn ++ "fields/fields12.png")
  f13 <- loadTex (fn ++ "fields/fields13.png")
  f14 <- loadTex (fn ++ "fields/fields14.png")
  f15 <- loadTex (fn ++ "fields/fields15.png")
  f16 <- loadTex (fn ++ "fields/fields16.png")
  f17 <- loadTex (fn ++ "fields/fields17.png")
  f18 <- loadTex (fn ++ "fields/fields18.png")
  f19 <- loadTex (fn ++ "fields/fields19.png")
  f20 <- loadTex (fn ++ "fields/fields20.png")
  f21 <- loadTex (fn ++ "fields/fields21.png")
  f22 <- loadTex (fn ++ "fields/fields22.png")
  f23 <- loadTex (fn ++ "fields/fields23.png")
  f24 <- loadTex (fn ++ "fields/fields24.png")
  f25 <- loadTex (fn ++ "fields/fields25.png")
  f26 <- loadTex (fn ++ "fields/fields26.png")
  c0 <- loadTex (fn ++ "crags/crags0.png")
  c1 <- loadTex (fn ++ "crags/crags1.png")
  c2 <- loadTex (fn ++ "crags/crags2.png")
  c3 <- loadTex (fn ++ "crags/crags3.png")
  c4 <- loadTex (fn ++ "crags/crags4.png")
  c5 <- loadTex (fn ++ "crags/crags5.png")
  c6 <- loadTex (fn ++ "crags/crags6.png")
  c7 <- loadTex (fn ++ "crags/crags7.png")
  c8 <- loadTex (fn ++ "crags/crags8.png")
  c9 <- loadTex (fn ++ "crags/crags9.png")
  c10 <- loadTex (fn ++ "crags/crags10.png")
  c11 <- loadTex (fn ++ "crags/crags11.png")
  c12 <- loadTex (fn ++ "crags/crags12.png")
  c13 <- loadTex (fn ++ "crags/crags13.png")
  c14 <- loadTex (fn ++ "crags/crags14.png")
  c15 <- loadTex (fn ++ "crags/crags15.png")
  c16 <- loadTex (fn ++ "crags/crags16.png")
  c17 <- loadTex (fn ++ "crags/crags17.png")
  c18 <- loadTex (fn ++ "crags/crags18.png")
  c19 <- loadTex (fn ++ "crags/crags19.png")
  c20 <- loadTex (fn ++ "crags/crags20.png")
  c21 <- loadTex (fn ++ "crags/crags21.png")
  c22 <- loadTex (fn ++ "crags/crags22.png")
  c23 <- loadTex (fn ++ "crags/crags23.png")
  c24 <- loadTex (fn ++ "crags/crags24.png")
  c25 <- loadTex (fn ++ "crags/crags25.png")
  c26 <- loadTex (fn ++ "crags/crags26.png")
  w0 <- loadTex (fn ++ "waste/waste0.png")
  w1 <- loadTex (fn ++ "waste/waste1.png")
  w2 <- loadTex (fn ++ "waste/waste2.png")
  w3 <- loadTex (fn ++ "waste/waste3.png")
  w4 <- loadTex (fn ++ "waste/waste4.png")
  w5 <- loadTex (fn ++ "waste/waste5.png")
  w6 <- loadTex (fn ++ "waste/waste6.png")
  w7 <- loadTex (fn ++ "waste/waste7.png")
  w8 <- loadTex (fn ++ "waste/waste8.png")
  w9 <- loadTex (fn ++ "waste/waste9.png")
  w10 <- loadTex (fn ++ "waste/waste10.png")
  w11 <- loadTex (fn ++ "waste/waste11.png")
  w12 <- loadTex (fn ++ "waste/waste12.png")
  w13 <- loadTex (fn ++ "waste/waste13.png")
  w14 <- loadTex (fn ++ "waste/waste14.png")
  w15 <- loadTex (fn ++ "waste/waste15.png")
  w16 <- loadTex (fn ++ "waste/waste16.png")
  w17 <- loadTex (fn ++ "waste/waste17.png")
  w18 <- loadTex (fn ++ "waste/waste18.png")
  w19 <- loadTex (fn ++ "waste/waste19.png")
  w20 <- loadTex (fn ++ "waste/waste20.png")
  w21 <- loadTex (fn ++ "waste/waste21.png")
  w22 <- loadTex (fn ++ "waste/waste22.png")
  w23 <- loadTex (fn ++ "waste/waste23.png")
  w24 <- loadTex (fn ++ "waste/waste24.png")
  w25 <- loadTex (fn ++ "waste/waste25.png")
  w26 <- loadTex (fn ++ "waste/waste26.png")

  s1 <- loadTex (fn ++ "sea/sea1.png")
  i1 <- loadTex (fn ++ "ice/ice1.png")

  return ([[p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p18, p19, p20, p21, p22, p23, p24, p25, p26], [f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f18, f19, f20, f21, f22, f23, f24, f25, f26], [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c18, c19, c20, c21, c22, c23, c24, c25, c26], [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w18, w19, w20, w21, w22, w23, w24, w25, w26], [s1], [i1]])

loadWTextures :: String -> IO ([TextureObject])
loadWTextures fn = do
  t0 <- loadTex (fn ++ "util/wcursor.png")
  t1 <- loadTex (fn ++ "sea/wsea.png")
  t2 <- loadTex (fn ++ "ice/wice.png")
  t3 <- loadTex (fn ++ "plains/wplains.png")
  t4 <- loadTex (fn ++ "fields/wfields.png")
  t5 <- loadTex (fn ++ "crags/wcrags.png")
  t6 <- loadTex (fn ++ "waste/wwaste.png")
  t7 <- loadTex (fn ++ "shallows/wshallows.png")
  t8 <- loadTex (fn ++ "steeps/wsteeps.png")

  return ([t0, t1, t2, t3, t4, t5, t6, t7, t8])

loadTex :: String -> IO GL.TextureObject
loadTex fn = do
  t <- either error id <$> readTexture fn
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
  return t

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

