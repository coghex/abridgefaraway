module GLUtil.Font where
-- we define how to make fonts appear

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT (($=))
import Graphics.GLU
import qualified Codec.Picture as JT
import Graphics.Rendering.OpenGL.GL.Texturing
import System.Exit

import GLUtil.Util
import GLUtil.Textures
import GLUtil.JuicyTextures

data Font = Font { fontfilepath :: String
                 , texs         :: [GL.TextureObject] } deriving (Show, Eq)

newFont :: String -> [GL.TextureObject] -> Font
newFont fp texs = Font { fontfilepath = fp
                       , texs         = texs }

nullFont :: Font
nullFont        = Font { fontfilepath = ""
                       , texs         = [] }

makeFonts :: [[GL.TextureObject]] -> [Font]
makeFonts texs = [nullFont, (newFont "data/fonts/sm/smalph" (texs!!1))]

loadFont :: String -> IO (Font)
loadFont fp = do
  texs <- loadNTexs 80 fpfull []
  return $ newFont fpfull texs
  where fpfull = "data/fonts/" ++ fp

loadNTexs :: Int -> String -> [GL.TextureObject] -> IO ([GL.TextureObject])
loadNTexs 0 _  texs = return texs
loadNTexs n fp texs = do
  x <- loadTex fpfull
  loadNTexs (n-1) fp (x : texs)
  where fpfull = fp ++ (show (n-1)) ++ ".png"

loadFontTextures :: String -> IO ([[GL.TextureObject]])
loadFontTextures str = do
  smfont <- loadNTexs 80 smfp []
  return $ [[], smfont]
  where smfp   = str ++ "sm/smalph"

loadTex :: String -> IO (GL.TextureObject)
loadTex fp = do
  t <- either error id <$> readTexture fp
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
  return t

drawFont :: Font -> (Int, Int) -> String -> IO ()
drawFont font pos str = do
  sceneSetup
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  drawFontLoop font pos str

drawFontLoop :: Font -> (Int, Int) -> String -> IO ()
drawFontLoop _    (_, _) []      = glFlush
drawFontLoop font (x, y) (l:str) = do
  withTextures2D [(ts!!n)] $ drawFontSquare ts l thiszoom x y
  drawFontLoop font ((x+1), y) str
  where thiszoom = 50
        ts       = texs font
        n        = findLetter l

drawFontSquare :: [GL.TextureObject] -> Char -> Float -> Int -> Int -> IO ()
drawFontSquare texs c zoom x y = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - (fromIntegral 60))) (2*((fromIntegral y) - ((fromIntegral 45)))) (-zoom)
  glColor3f 1.0 1.0 1.0
  drawSquare

-- will find which number the current char maps to
findLetter :: Char -> Int
findLetter 'a' = 0
findLetter 'b' = 1
findLetter 'c' = 2
findLetter 'd' = 3
findLetter c   = 0
