module Util ( bitmapLoad, Image(..), initTexs ) where

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
import Graphics.GLU
import Graphics.GLUtil
import qualified Codec.Picture as JT
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GL.Texturing
import System.Exit

import Paths
import Font


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

initTexs :: GLFW.Window -> IO ([GLuint], [TextureObject], [[GLuint]])
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
  loadWTextures "maps/"

loadZTextures :: String -> IO ([[GLuint]])
loadZTextures fn = do
  p1 <- loadGLTextures (fn ++ "plains/plains001.bmp")
  f1 <- loadGLTextures (fn ++ "fields/fields001.bmp")
  f2 <- loadGLTextures (fn ++ "fields/fields002.bmp")
  f3 <- loadGLTextures (fn ++ "fields/fields003.bmp")
  f4 <- loadGLTextures (fn ++ "fields/fields004.bmp")
  f5 <- loadGLTextures (fn ++ "fields/fields005.bmp")
  f6 <- loadGLTextures (fn ++ "fields/fields006.bmp")
  f7 <- loadGLTextures (fn ++ "fields/fields007.bmp")
  c1 <- loadGLTextures (fn ++ "crags/crags001.bmp")
  b1 <- loadGLTextures (fn ++ "beach/beach001.bmp")
  s1 <- loadGLTextures (fn ++ "sea/sea001.bmp")
  
  return ([[p1], [f1, f2, f3, f4, f5, f6, f7], [c1], [b1], [s1]])

loadWTextures :: String -> IO ([GLuint], [TextureObject], [[GLuint]])
loadWTextures fn = do
  t1 <- loadGLTextures (fn ++ "plains/worldplains.bmp")
  t2 <- loadGLTextures (fn ++ "fields/worldfields.bmp")
  t3 <- loadGLTextures (fn ++ "crags/worldcrags.bmp")
  t4 <- loadGLTextures (fn ++ "beach/worldbeach.bmp")
  t5 <- loadGLTextures (fn ++ "imp/worldimp.bmp")
  t6 <- loadGLTextures (fn ++ "sea/worldsea.bmp")
  t7 <- loadGLTextures (fn ++ "sea/worldcoast.bmp")
  t8 <- loadGLTextures (fn ++ "sea/worldice.bmp")
  t9 <- runMaybeT $ loadGLPngTextures ("data/wcursor.png")
  fonttex <- loadCharacter "data/fonts/amatic/AmaticSC-Regular.ttf" 'A' 24
  zt <- loadZTextures fn
  
  return ([t1, t2, t3, t4, t5, t6, t7, t8, (fromJust t9)], [fonttex], zt)

loadGLTextures :: String -> IO GLuint
loadGLTextures fn = do
  fp <- getDataFileName (fn)
  putStrLn $ "loading texture: " ++ fp
  Just (Image w h pd) <- bitmapLoad fp
  tex <- alloca $ \p -> do
    glGenTextures 1 p
    peek p
  let (ptr, off, _) = BSI.toForeignPtr pd
  withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
    glBindTexture GL_TEXTURE_2D tex
    glTexImage2D GL_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h) 0 GL_RGB GL_UNSIGNED_BYTE
      p'
    let glLinear = fromIntegral GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER glLinear
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER glLinear
  return tex

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

loadGLPngTextures :: MonadIO m => String -> MaybeT m (GLuint)
loadGLPngTextures fn = do
  fp <- liftIO $ JT.readPng fn
  (JT.Image texWidth texHeight texData) <- MaybeT $ case fp of
    (Right (JT.ImageRGBA8 i)) -> return $ Just i
    (Left s)                  -> liftIO (print s) >> return Nothing
    _                         -> return Nothing
  texID <- liftIO $ alloca $ \texIDPtr -> do
    glGenTextures 1 texIDPtr
    peek texIDPtr
  return texID
  --return ()

bitmapLoad :: FilePath -> IO (Maybe Image)
bitmapLoad f = do
  bs <- BS.readFile f
  case runGet getBitmap bs of
    Left  err -> putStrLn err >> return Nothing
    Right i@(Image _ _ bytes) -> do
      let (ptr, offset, len) = BSI.toForeignPtr bytes
      withForeignPtr ptr $ bgr2rgb len offset
      return $! Just i

-- | Returns a bitmap in bgr format
getBitmap :: Get Image
getBitmap = do
  skip 18
  width   <- getWord32le
  height  <- getWord32le
  _planes <- getWord16le
  bpp     <- getWord16le
  let size = fromIntegral $
              width * height * (fromIntegral (bpp `div` 8) :: Word32) :: Int
  skip 24
  bgrBytes <- getByteString size
  return $! Image (fromIntegral width)
                  (fromIntegral height)
                  bgrBytes

bgr2rgb :: Int -> Int -> Ptr Word8 -> IO ()
bgr2rgb n o p = do
  mapM_ (\i -> do
    b <- peekElemOff p' (i+0)
    r <- peekElemOff p' (i+2)
    pokeElemOff p' (i+0) (r::Word8)
    pokeElemOff p' (i+2) (b::Word8))
    [0,3..n-3]
  where
  p' = p `plusPtr` o


