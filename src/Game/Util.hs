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
import GLUtil.Textures
import GLUtil.JuicyTextures
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

initTexs :: GLFW.Window -> IO ([TextureObject], [[TextureObject]], [[TextureObject]], [[TextureObject]])
initTexs win = do
  glEnable GL_TEXTURE_2D
  glShadeModel GL_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST
  (w, h) <- GLFW.getFramebufferSize win
  resizeScene win w h
  wtex <- loadWTextures "data/biome/"
  ztex <- loadZTextures "data/zone/"
  utex <- loadUTextures "data/util/"
  unittex <- loadUnitTextures "data/unit/"
  return (wtex, ztex, utex, unittex)

loadZTextures :: String -> IO ([[TextureObject]])
loadZTextures fn = do
  zc <- loadTex (fn ++ "util/zcursor.png")
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
  p27 <- loadTex (fn ++ "plains/plains27.png")
  p28 <- loadTex (fn ++ "plains/plains28.png")
  p29 <- loadTex (fn ++ "plains/plains29.png")
  p30 <- loadTex (fn ++ "plains/plains30.png")
  p31 <- loadTex (fn ++ "plains/plains31.png")
  p32 <- loadTex (fn ++ "plains/plains32.png")
  p33 <- loadTex (fn ++ "plains/plains33.png")
  p34 <- loadTex (fn ++ "plains/plains34.png")
  p35 <- loadTex (fn ++ "plains/plains35.png")
  p36 <- loadTex (fn ++ "plains/plains36.png")
  p37 <- loadTex (fn ++ "plains/plains37.png")
  p38 <- loadTex (fn ++ "plains/plains38.png")
  p39 <- loadTex (fn ++ "plains/plains39.png")
  p40 <- loadTex (fn ++ "plains/plains40.png")
  p41 <- loadTex (fn ++ "plains/plains41.png")
  p42 <- loadTex (fn ++ "plains/plains42.png")
  p43 <- loadTex (fn ++ "plains/plains43.png")
  p44 <- loadTex (fn ++ "plains/plains44.png")
  p45 <- loadTex (fn ++ "plains/plains45.png")
  p46 <- loadTex (fn ++ "plains/plains46.png")
  p47 <- loadTex (fn ++ "plains/plains47.png")
  p48 <- loadTex (fn ++ "plains/plains48.png")
  p49 <- loadTex (fn ++ "plains/plains49.png")
  p50 <- loadTex (fn ++ "plains/plains50.png")
  p51 <- loadTex (fn ++ "plains/plains51.png")
  p52 <- loadTex (fn ++ "plains/plains52.png")
  p53 <- loadTex (fn ++ "plains/plains53.png")
  p54 <- loadTex (fn ++ "plains/plains54.png")
  p55 <- loadTex (fn ++ "plains/plains55.png")
  p56 <- loadTex (fn ++ "plains/plains56.png")
  p57 <- loadTex (fn ++ "plains/plains57.png")
  p58 <- loadTex (fn ++ "plains/plains58.png")
  p59 <- loadTex (fn ++ "plains/plains59.png")
  p60 <- loadTex (fn ++ "plains/plains60.png")
  p61 <- loadTex (fn ++ "plains/plains61.png")
  p62 <- loadTex (fn ++ "plains/plains62.png")
  p63 <- loadTex (fn ++ "plains/plains63.png")
  p64 <- loadTex (fn ++ "plains/plains64.png")
  p65 <- loadTex (fn ++ "plains/plains65.png")
  p66 <- loadTex (fn ++ "plains/plains66.png")
  p67 <- loadTex (fn ++ "plains/plains67.png")
  p68 <- loadTex (fn ++ "plains/plains68.png")
  p69 <- loadTex (fn ++ "plains/plains69.png")
  p70 <- loadTex (fn ++ "plains/plains70.png")
  p71 <- loadTex (fn ++ "plains/plains71.png")
  p72 <- loadTex (fn ++ "plains/plains72.png")
  p73 <- loadTex (fn ++ "plains/plains73.png")
  p74 <- loadTex (fn ++ "plains/plains74.png")
  p75 <- loadTex (fn ++ "plains/plains75.png")
  p76 <- loadTex (fn ++ "plains/plains76.png")
  p77 <- loadTex (fn ++ "plains/plains77.png")
  p78 <- loadTex (fn ++ "plains/plains78.png")
  p79 <- loadTex (fn ++ "plains/plains79.png")
  p80 <- loadTex (fn ++ "plains/plains80.png")
  p81 <- loadTex (fn ++ "plains/plains81.png")
  p82 <- loadTex (fn ++ "plains/plains82.png")
  p83 <- loadTex (fn ++ "plains/plains83.png")
  p84 <- loadTex (fn ++ "plains/plains84.png")
  p85 <- loadTex (fn ++ "plains/plains85.png")
  p86 <- loadTex (fn ++ "plains/plains86.png")
  p87 <- loadTex (fn ++ "plains/plains87.png")
  p88 <- loadTex (fn ++ "plains/plains88.png")
  p89 <- loadTex (fn ++ "plains/plains89.png")
  p90 <- loadTex (fn ++ "plains/plains90.png")
  p91 <- loadTex (fn ++ "plains/plains91.png")
  p92 <- loadTex (fn ++ "plains/plains92.png")
  p93 <- loadTex (fn ++ "plains/plains93.png")
  p94 <- loadTex (fn ++ "plains/plains94.png")
  p95 <- loadTex (fn ++ "plains/plains95.png")
  p96 <- loadTex (fn ++ "plains/plains96.png")
  p97 <- loadTex (fn ++ "plains/plains97.png")
  p98 <- loadTex (fn ++ "plains/plains98.png")
  p99 <- loadTex (fn ++ "plains/plains99.png")
  p100 <- loadTex (fn ++ "plains/plains100.png")
  p101 <- loadTex (fn ++ "plains/plains101.png")
  p102 <- loadTex (fn ++ "plains/plains102.png")
  p103 <- loadTex (fn ++ "plains/plains103.png")
  p104 <- loadTex (fn ++ "plains/plains104.png")
  p105 <- loadTex (fn ++ "plains/plains105.png")
  p106 <- loadTex (fn ++ "plains/plains106.png")
  p107 <- loadTex (fn ++ "plains/plains107.png")
  p108 <- loadTex (fn ++ "plains/plains108.png")
  p109 <- loadTex (fn ++ "plains/plains109.png")
  p110 <- loadTex (fn ++ "plains/plains110.png")

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
  c27 <- loadTex (fn ++ "crags/crags27.png")
  c28 <- loadTex (fn ++ "crags/crags28.png")
  c29 <- loadTex (fn ++ "crags/crags29.png")
  c30 <- loadTex (fn ++ "crags/crags30.png")
  c31 <- loadTex (fn ++ "crags/crags31.png")
  c32 <- loadTex (fn ++ "crags/crags32.png")
  c33 <- loadTex (fn ++ "crags/crags33.png")
  c34 <- loadTex (fn ++ "crags/crags34.png")
  c35 <- loadTex (fn ++ "crags/crags35.png")
  c36 <- loadTex (fn ++ "crags/crags36.png")
  c37 <- loadTex (fn ++ "crags/crags37.png")
  c38 <- loadTex (fn ++ "crags/crags38.png")
  c39 <- loadTex (fn ++ "crags/crags39.png")
  c40 <- loadTex (fn ++ "crags/crags40.png")
  c41 <- loadTex (fn ++ "crags/crags41.png")
  c42 <- loadTex (fn ++ "crags/crags42.png")
  c43 <- loadTex (fn ++ "crags/crags43.png")
  c44 <- loadTex (fn ++ "crags/crags44.png")
  c45 <- loadTex (fn ++ "crags/crags45.png")
  c46 <- loadTex (fn ++ "crags/crags46.png")
  c47 <- loadTex (fn ++ "crags/crags47.png")
  c48 <- loadTex (fn ++ "crags/crags48.png")
  c49 <- loadTex (fn ++ "crags/crags49.png")
  c50 <- loadTex (fn ++ "crags/crags50.png")
  c51 <- loadTex (fn ++ "crags/crags51.png")
  c52 <- loadTex (fn ++ "crags/crags52.png")
  c53 <- loadTex (fn ++ "crags/crags53.png")
  c54 <- loadTex (fn ++ "crags/crags54.png")
  c55 <- loadTex (fn ++ "crags/crags55.png")
  c56 <- loadTex (fn ++ "crags/crags56.png")
  c57 <- loadTex (fn ++ "crags/crags57.png")
  c58 <- loadTex (fn ++ "crags/crags58.png")
  c59 <- loadTex (fn ++ "crags/crags59.png")
  c60 <- loadTex (fn ++ "crags/crags60.png")
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
  v0 <- loadTex (fn ++ "valley/valley0.png")
  i1 <- loadTex (fn ++ "ice/ice1.png")
  sh0 <- loadTex (fn ++ "shallows/shallows0.png")
  st0 <- loadTex (fn ++ "steeps/steeps0.png")
  pk0 <- loadTex (fn ++ "peaks/peaks0.png")
  n0 <- loadTex (fn ++ "util/null0.png")
  d0 <- loadTex (fn ++ "deeps/deeps0.png")

  return ([[zc], [s1], [v0], [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47, p48, p49, p50, p51, p52, p53, p54, p55, p56, p57, p58, p59, p60, p61, p62, p63, p64, p65, p66, p67, p68, p69, p70, p71, p72, p73, p74, p75, p76, p77, p78, p79, p80, p81, p82, p83, p84, p85, p86, p87, p88, p89, p90, p91, p92, p93, p94, p95, p96, p97, p98, p99, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110], [f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f18, f19, f20, f21, f22, f23, f24, f25, f26], [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30, c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44, c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58, c59, c60], [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w18, w19, w20, w21, w22, w23, w24, w25, w26], [sh0], [st0], [pk0], [n0], [i1], [d0]])
loadWTextures :: String -> IO ([TextureObject])
loadWTextures fn = do
  t0  <- loadTex (fn ++ "util/wcursor.png")
  t1  <- loadTex (fn ++ "sea/wsea.png")
  t2  <- loadTex (fn ++ "valley/wvalley.png")
  t3  <- loadTex (fn ++ "plains/wplains.png")
  t4  <- loadTex (fn ++ "fields/wfields.png")
  t5  <- loadTex (fn ++ "crags/wcrags.png")
  t6  <- loadTex (fn ++ "waste/wwaste.png")
  t7  <- loadTex (fn ++ "shallows/wshallows.png")
  t8  <- loadTex (fn ++ "steeps/wsteeps.png")
  t9  <- loadTex (fn ++ "peaks/wpeaks.png")
  t10 <- loadTex (fn ++ "null/null.png")
  t11 <- loadTex (fn ++ "ice/wice.png")
  t12 <- loadTex (fn ++ "deeps/wdeeps.png")
  t13 <- loadTex (fn ++ "sea/sean.png")
  t14 <- loadTex (fn ++ "sea/seanw.png")
  t15 <- loadTex (fn ++ "sea/seaw.png")
  t16 <- loadTex (fn ++ "sea/seasw.png")
  t17 <- loadTex (fn ++ "sea/seas.png")
  t18 <- loadTex (fn ++ "sea/sease.png")
  t19 <- loadTex (fn ++ "sea/seae.png")
  t20 <- loadTex (fn ++ "sea/seane.png")

  return ([t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20])

loadUnitTextures :: String -> IO ([[TextureObject]])
loadUnitTextures fn = do
  n0 <- loadTex (fn ++ "util/null0.png")
  me0  <- loadTex (fn ++ "minion/minione0.png")
  me1  <- loadTex (fn ++ "minion/minione1.png")
  me2  <- loadTex (fn ++ "minion/minione2.png")
  me3  <- loadTex (fn ++ "minion/minione3.png")
  me4  <- loadTex (fn ++ "minion/minione4.png")

  return ([[n0], [me0, me1, me2, me3, me4]])

loadUTextures :: String -> IO ([[TextureObject]])
loadUTextures fn = do
  b0  <- loadTex (fn ++ "box/box.png")
  b1  <- loadTex (fn ++ "box/boxnw.png")
  b2  <- loadTex (fn ++ "box/boxn.png")
  b3  <- loadTex (fn ++ "box/boxne.png")
  b4  <- loadTex (fn ++ "box/boxe.png")
  b5  <- loadTex (fn ++ "box/boxse.png")
  b6  <- loadTex (fn ++ "box/boxs.png")
  b7  <- loadTex (fn ++ "box/boxsw.png")
  b8  <- loadTex (fn ++ "box/boxw.png")

  return ([[], [b0, b1, b2, b3, b4, b5, b6, b7, b8]])

loadTex :: String -> IO GL.TextureObject
loadTex fn = do
  t <- either error id <$> readTexture fn
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
  return t

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

