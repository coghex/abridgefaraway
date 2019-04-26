module GLUtil.TexLoad where
-- the textures to load are defined, eventually we want
-- lua to call these so mods can add new textures

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLU as GLU
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.UI.GLFW as GLFW
import GLUtil.Textures
import GLUtil.JuicyTextures

-- loads a texture into a textureobject
loadTex :: String -> IO GL.TextureObject
loadTex fn = do
  t <- either error id <$> readTexture fn
  GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap GL.$= (GL.Mirrored, GL.ClampToEdge)
  return t

-- load a number of texs if they are labeled numerically
loadNTexs :: Int -> String -> [GL.TextureObject] -> IO ([GL.TextureObject])
loadNTexs 0 _  texs = return texs
loadNTexs n fp texs = do
  x <- loadTex fpfull
  loadNTexs (n-1) fp (x : texs)
  where fpfull = fp ++ (show (n-1)) ++ ".png"

-- loads all of the world map textures
loadWorldTextures :: String -> IO ([GL.TextureObject])
loadWorldTextures fn = do
  t0  <- loadTex (fn ++ "util/wcursor.png")
  t1  <- loadTex (fn ++ "sea/wsea.png")
  t2  <- loadTex (fn ++ "shallows/wshallows.png")
  t3  <- loadTex (fn ++ "deeps/wdeeps.png")
  t4  <- loadTex (fn ++ "valley/wvalley.png")
  t5  <- loadTex (fn ++ "crags/wcrags.png")
  t6  <- loadTex (fn ++ "plains/wplains.png")
  t7  <- loadTex (fn ++ "fields/wfields.png")
  t8  <- loadTex (fn ++ "wastes/wwastes.png")
  t9  <- loadTex (fn ++ "steeps/wsteeps.png")
  t10 <- loadTex (fn ++ "peaks/wpeaks.png")
  t11 <- loadTex (fn ++ "null/null.png")
  t12 <- loadTex (fn ++ "ice/wice.png")
  t13 <- loadTex (fn ++ "sea/wseacurrent.png")
  t14 <- loadTex (fn ++ "sea/sean.png")
  t15 <- loadTex (fn ++ "sea/seanw.png")
  t16 <- loadTex (fn ++ "sea/seaw.png")
  t17 <- loadTex (fn ++ "sea/seasw.png")
  t18 <- loadTex (fn ++ "sea/seas.png")
  t19 <- loadTex (fn ++ "sea/sease.png")
  t20 <- loadTex (fn ++ "sea/seae.png")
  t21 <- loadTex (fn ++ "sea/seane.png")

  return ([t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21])
