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
  wtexs <- loadWTextures "data/maps/"
  ztexs <- loadZTextures "data/maps/"
  return (wtexs, ztexs)

sceneSetup :: IO ()
sceneSetup = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT

loadZTextures :: String -> IO ([[TextureObject]])
loadZTextures fn = do
  path0 <- loadGLZTextures (fn ++ "paths/paths0.png")
  path1 <- loadGLZTextures (fn ++ "paths/paths1.png")
  path2 <- loadGLZTextures (fn ++ "paths/paths2.png")
  path3 <- loadGLZTextures (fn ++ "paths/paths3.png")
  path4 <- loadGLZTextures (fn ++ "paths/paths4.png")
  path5 <- loadGLZTextures (fn ++ "paths/paths5.png")
  path6 <- loadGLZTextures (fn ++ "paths/paths6.png")
  path7 <- loadGLZTextures (fn ++ "paths/paths7.png")
  path8 <- loadGLZTextures (fn ++ "paths/paths8.png")
  path9 <- loadGLZTextures (fn ++ "paths/paths9.png")
  path10 <- loadGLZTextures (fn ++ "paths/paths10.png")
  path11 <- loadGLZTextures (fn ++ "paths/paths11.png")
  path12 <- loadGLZTextures (fn ++ "paths/paths12.png")
  path13 <- loadGLZTextures (fn ++ "paths/paths13.png")
  path14 <- loadGLZTextures (fn ++ "paths/paths14.png")
  path15 <- loadGLZTextures (fn ++ "paths/paths15.png")
  path16 <- loadGLZTextures (fn ++ "paths/paths16.png")
  path17 <- loadGLZTextures (fn ++ "paths/paths17.png")
  path18 <- loadGLZTextures (fn ++ "paths/paths18.png")
  path19 <- loadGLZTextures (fn ++ "paths/paths19.png")
  path20 <- loadGLZTextures (fn ++ "paths/paths20.png")
  path21 <- loadGLZTextures (fn ++ "paths/paths21.png")
  path22 <- loadGLZTextures (fn ++ "paths/paths22.png")
  path23 <- loadGLZTextures (fn ++ "paths/paths23.png")
  path24 <- loadGLZTextures (fn ++ "paths/paths24.png")
  path25 <- loadGLZTextures (fn ++ "paths/paths25.png")
  path26 <- loadGLZTextures (fn ++ "paths/paths26.png")
  path27 <- loadGLZTextures (fn ++ "paths/paths27.png")
  path28 <- loadGLZTextures (fn ++ "paths/paths28.png")
  path29 <- loadGLZTextures (fn ++ "paths/paths29.png")
  path30 <- loadGLZTextures (fn ++ "paths/paths30.png")
  path31 <- loadGLZTextures (fn ++ "paths/paths31.png")
  path32 <- loadGLZTextures (fn ++ "paths/paths32.png")
  path33 <- loadGLZTextures (fn ++ "paths/paths33.png")
  path34 <- loadGLZTextures (fn ++ "paths/paths34.png")
  path35 <- loadGLZTextures (fn ++ "paths/paths35.png")
  path36 <- loadGLZTextures (fn ++ "paths/paths36.png")
  path37 <- loadGLZTextures (fn ++ "paths/paths37.png")
  path38 <- loadGLZTextures (fn ++ "paths/paths38.png")
  path39 <- loadGLZTextures (fn ++ "paths/paths39.png")
  path40 <- loadGLZTextures (fn ++ "paths/paths40.png")
  path41 <- loadGLZTextures (fn ++ "paths/paths41.png")
  p0 <- loadGLZTextures (fn ++ "plains/plains0.png")
  p1 <- loadGLZTextures (fn ++ "plains/plains1.png")
  p2 <- loadGLZTextures (fn ++ "plains/plains2.png")
  p3 <- loadGLZTextures (fn ++ "plains/plains3.png")
  p4 <- loadGLZTextures (fn ++ "plains/plains4.png")
  p5 <- loadGLZTextures (fn ++ "plains/plains5.png")
  p6 <- loadGLZTextures (fn ++ "plains/plains6.png")
  p7 <- loadGLZTextures (fn ++ "plains/plains7.png")
  p8 <- loadGLZTextures (fn ++ "plains/plains8.png")
  p9 <- loadGLZTextures (fn ++ "plains/plains9.png")
  p10 <- loadGLZTextures (fn ++ "plains/plains10.png")
  p11 <- loadGLZTextures (fn ++ "plains/plains11.png")
  p12 <- loadGLZTextures (fn ++ "plains/plains12.png")
  p13 <- loadGLZTextures (fn ++ "plains/plains13.png")
  p14 <- loadGLZTextures (fn ++ "plains/plains14.png")
  p15 <- loadGLZTextures (fn ++ "plains/plains15.png")
  p16 <- loadGLZTextures (fn ++ "plains/plains16.png")
  p17 <- loadGLZTextures (fn ++ "plains/plains17.png")
  p18 <- loadGLZTextures (fn ++ "plains/plains18.png")
  p19 <- loadGLZTextures (fn ++ "plains/plains19.png")
  p20 <- loadGLZTextures (fn ++ "plains/plains20.png")
  p21 <- loadGLZTextures (fn ++ "plains/plains21.png")
  p22 <- loadGLZTextures (fn ++ "plains/plains22.png")
  p23 <- loadGLZTextures (fn ++ "plains/plains23.png")
  p24 <- loadGLZTextures (fn ++ "plains/plains24.png")
  p25 <- loadGLZTextures (fn ++ "plains/plains25.png")
  p26 <- loadGLZTextures (fn ++ "plains/plains26.png")
  f0 <- loadGLZTextures (fn ++ "fields/fields0.png")
  f1 <- loadGLZTextures (fn ++ "fields/fields1.png")
  f2 <- loadGLZTextures (fn ++ "fields/fields2.png")
  f3 <- loadGLZTextures (fn ++ "fields/fields3.png")
  f4 <- loadGLZTextures (fn ++ "fields/fields4.png")
  f5 <- loadGLZTextures (fn ++ "fields/fields5.png")
  f6 <- loadGLZTextures (fn ++ "fields/fields6.png")
  f7 <- loadGLZTextures (fn ++ "fields/fields7.png")
  f8 <- loadGLZTextures (fn ++ "fields/fields8.png")
  f9 <- loadGLZTextures (fn ++ "fields/fields9.png")
  f10 <- loadGLZTextures (fn ++ "fields/fields10.png")
  f11 <- loadGLZTextures (fn ++ "fields/fields11.png")
  f12 <- loadGLZTextures (fn ++ "fields/fields12.png")
  f13 <- loadGLZTextures (fn ++ "fields/fields13.png")
  f14 <- loadGLZTextures (fn ++ "fields/fields14.png")
  f15 <- loadGLZTextures (fn ++ "fields/fields15.png")
  f16 <- loadGLZTextures (fn ++ "fields/fields16.png")
  f17 <- loadGLZTextures (fn ++ "fields/fields17.png")
  f18 <- loadGLZTextures (fn ++ "fields/fields18.png")
  f19 <- loadGLZTextures (fn ++ "fields/fields19.png")
  f20 <- loadGLZTextures (fn ++ "fields/fields20.png")
  f21 <- loadGLZTextures (fn ++ "fields/fields21.png")
  f22 <- loadGLZTextures (fn ++ "fields/fields22.png")
  f23 <- loadGLZTextures (fn ++ "fields/fields23.png")
  f24 <- loadGLZTextures (fn ++ "fields/fields24.png")
  f25 <- loadGLZTextures (fn ++ "fields/fields25.png")
  f26 <- loadGLZTextures (fn ++ "fields/fields26.png")
  c0 <- loadGLZTextures (fn ++ "crags/crags0.png")
  c1 <- loadGLZTextures (fn ++ "crags/crags1.png")
  c2 <- loadGLZTextures (fn ++ "crags/crags2.png")
  c3 <- loadGLZTextures (fn ++ "crags/crags3.png")
  c4 <- loadGLZTextures (fn ++ "crags/crags4.png")
  c5 <- loadGLZTextures (fn ++ "crags/crags5.png")
  c6 <- loadGLZTextures (fn ++ "crags/crags6.png")
  c7 <- loadGLZTextures (fn ++ "crags/crags7.png")
  c8 <- loadGLZTextures (fn ++ "crags/crags8.png")
  c9 <- loadGLZTextures (fn ++ "crags/crags9.png")
  c10 <- loadGLZTextures (fn ++ "crags/crags10.png")
  c11 <- loadGLZTextures (fn ++ "crags/crags11.png")
  c12 <- loadGLZTextures (fn ++ "crags/crags12.png")
  c13 <- loadGLZTextures (fn ++ "crags/crags13.png")
  c14 <- loadGLZTextures (fn ++ "crags/crags14.png")
  c15 <- loadGLZTextures (fn ++ "crags/crags15.png")
  c16 <- loadGLZTextures (fn ++ "crags/crags16.png")
  c17 <- loadGLZTextures (fn ++ "crags/crags17.png")
  c18 <- loadGLZTextures (fn ++ "crags/crags18.png")
  c19 <- loadGLZTextures (fn ++ "crags/crags19.png")
  c20 <- loadGLZTextures (fn ++ "crags/crags20.png")
  c21 <- loadGLZTextures (fn ++ "crags/crags21.png")
  c22 <- loadGLZTextures (fn ++ "crags/crags22.png")
  c23 <- loadGLZTextures (fn ++ "crags/crags23.png")
  c24 <- loadGLZTextures (fn ++ "crags/crags24.png")
  c25 <- loadGLZTextures (fn ++ "crags/crags25.png")
  c26 <- loadGLZTextures (fn ++ "crags/crags26.png")
  w0 <- loadGLZTextures (fn ++ "waste/waste0.png")
  w1 <- loadGLZTextures (fn ++ "waste/waste1.png")
  w2 <- loadGLZTextures (fn ++ "waste/waste2.png")
  w3 <- loadGLZTextures (fn ++ "waste/waste3.png")
  w4 <- loadGLZTextures (fn ++ "waste/waste4.png")
  w5 <- loadGLZTextures (fn ++ "waste/waste5.png")
  w6 <- loadGLZTextures (fn ++ "waste/waste6.png")
  w7 <- loadGLZTextures (fn ++ "waste/waste7.png")
  w8 <- loadGLZTextures (fn ++ "waste/waste8.png")
  w9 <- loadGLZTextures (fn ++ "waste/waste9.png")
  w10 <- loadGLZTextures (fn ++ "waste/waste10.png")
  w11 <- loadGLZTextures (fn ++ "waste/waste11.png")
  w12 <- loadGLZTextures (fn ++ "waste/waste12.png")
  w13 <- loadGLZTextures (fn ++ "waste/waste13.png")
  w14 <- loadGLZTextures (fn ++ "waste/waste14.png")
  w15 <- loadGLZTextures (fn ++ "waste/waste15.png")
  w16 <- loadGLZTextures (fn ++ "waste/waste16.png")
  w17 <- loadGLZTextures (fn ++ "waste/waste17.png")
  w18 <- loadGLZTextures (fn ++ "waste/waste18.png")
  w19 <- loadGLZTextures (fn ++ "waste/waste19.png")
  w20 <- loadGLZTextures (fn ++ "waste/waste20.png")
  w21 <- loadGLZTextures (fn ++ "waste/waste21.png")
  w22 <- loadGLZTextures (fn ++ "waste/waste22.png")
  w23 <- loadGLZTextures (fn ++ "waste/waste23.png")
  w24 <- loadGLZTextures (fn ++ "waste/waste24.png")
  w25 <- loadGLZTextures (fn ++ "waste/waste25.png")
  w26 <- loadGLZTextures (fn ++ "waste/waste26.png")

  s1 <- loadGLZTextures (fn ++ "sea/sea1.png")
  return ([[s1, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26], [s1, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26], [s1, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26], [s1, w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20, w21, w22, w23, w24, w25, w26], [path1, path2, path3, path4, path5, path6, path7, path8, path9, path10, path11, path12, path13, path14, path15, path16, path17, path18, path19, path20, path21, path22, path23, path24, path25, path26, path27, path28, path29, path30, path31, path32, path33, path34, path35, path36, path37, path38, path39, path40, path41], [s1]])

loadWTextures :: String -> IO ([TextureObject])
loadWTextures fn = do
  t0 <- loadGLZTextures (fn ++ "wcursor.png")
  t1 <- loadGLZTextures (fn ++ "plains/worldplains.png")
  t2 <- loadGLZTextures (fn ++ "fields/worldfields.png")
  t3 <- loadGLZTextures (fn ++ "crags/worldcrags.png")
  t4 <- loadGLZTextures (fn ++ "waste/worldwaste.png")
  t5 <- loadGLZTextures (fn ++ "sea/worldsea.png")
  t6 <- loadGLZTextures (fn ++ "ice/worldice.png")
  
  --return ([t1, t2, t3, t4, t5, t6, t7, t8], (zt!!0), zt)
  return ([t1, t2, t3, t4, t5, t6, t0])

loadGLZTextures :: String -> IO GL.TextureObject
loadGLZTextures fn = do
  t <- either error id <$> readTexture fn
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
  return t


fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x


