module GameInit where

import System.Random
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString.Internal as BSI
import System.Exit ( exitWith, ExitCode(..) )
import Foreign ( withForeignPtr, plusPtr, peek, alloca )
import Util
import Paths
import State

initGLTexs :: GLFW.Window -> IO [GLuint]
initGLTexs win = do
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

loadWTextures :: String -> IO [GLuint]
loadWTextures fn = do
  t1 <- loadGLTextures (fn ++ "plains/worldplains.bmp")
  t2 <- loadGLTextures (fn ++ "fields/worldfields.bmp")
  t3 <- loadGLTextures (fn ++ "crags/worldcrags.bmp")
  t4 <- loadGLTextures (fn ++ "beach/worldbeach.bmp")
  t5 <- loadGLTextures (fn ++ "imp/worldimp.bmp")
  t6 <- loadGLTextures (fn ++ "sea/worldsea.bmp")

  return [t1, t2, t3, t4, t5, t6]

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


randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR(min, max)

gameInit :: Env -> State -> IO ()
gameInit env state = do
  print "hi"
