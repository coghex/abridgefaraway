module GLinit where

import Graphics.GL
import Graphics.GLU
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString.Internal as BSI
import System.Exit ( exitWith, ExitCode(..) )
import Foreign ( withForeignPtr, plusPtr, peek, alloca )
import Util
import Paths

initGL :: GLFW.Window -> IO [GLuint]
initGL win = do
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
  t7 <- loadGLTextures (fn ++ "sea/worldcoast.bmp")
  t8 <- loadGLTextures (fn ++ "sea/worldice.bmp")

  return [t1, t2, t3, t4, t5, t6, t7, t8]

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

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()
