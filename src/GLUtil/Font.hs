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

beginDrawFont :: IO ()
beginDrawFont = do
  sceneSetup
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  
drawFont :: Font -> (Int, Int) -> String -> IO ()
drawFont font pos str = do
  drawFontLoop font (posr pos) str
  where posr = \(x,y) -> ((fromIntegral x), (fromIntegral y))

drawFontLoop :: Font -> (Float, Float) -> String -> IO ()
drawFontLoop _    (_, _) []      = glFlush
drawFontLoop font (x, y) (l:str) = do
  withTextures2D [(ts!!n)] $ drawFontSquare ts l thiszoom x y
  drawFontLoop font ((x+(nstep)), y) str
  where thiszoom = 50
        ts         = texs font
        (n, nsize) = findLetter l
        nstep      = (fromIntegral nsize) / 32.0

drawFontSquare :: [GL.TextureObject] -> Char -> Float -> Float -> Float -> IO ()
drawFontSquare texs c zoom x y = do
  glLoadIdentity
  glTranslatef (2*((x))) (2*((y))) (-zoom)
  glColor3f 1.0 1.0 1.0
  drawSquare

-- will find which number the current char maps to, and the size of that letter
findLetter :: Char -> (Int, Int)
findLetter 'a'  = ( 0, 12)
findLetter 'b'  = ( 1, 12)
findLetter 'c'  = ( 2, 12)
findLetter 'd'  = ( 3, 12)
findLetter 'e'  = ( 4, 12)
findLetter 'f'  = ( 5, 12)
findLetter 'g'  = ( 6, 12)
findLetter 'h'  = ( 7, 12)
findLetter 'i'  = ( 8,  8)
findLetter 'j'  = ( 9,  8)
findLetter 'k'  = (10, 12)
findLetter 'l'  = (11,  8)
findLetter 'm'  = (12, 16)
findLetter 'n'  = (13, 12)
findLetter 'o'  = (14, 12)
findLetter 'p'  = (15, 12)
findLetter 'q'  = (16, 12)
findLetter 'r'  = (17, 12)
findLetter 's'  = (18, 12)
findLetter 't'  = (19, 12)
findLetter 'u'  = (20, 12)
findLetter 'v'  = (21, 12)
findLetter 'w'  = (22, 16)
findLetter 'x'  = (23, 12)
findLetter 'y'  = (24, 12)
findLetter 'z'  = (25, 12)
findLetter '?'  = (26, 12)
findLetter '!'  = (27,  8)
findLetter '('  = (28,  8)
findLetter ')'  = (29,  8)
findLetter '\'' = (30,  8)
findLetter '"'  = (31,  8)
findLetter 'A'  = (32, 16)
findLetter 'B'  = (33, 16)
findLetter 'C'  = (34, 12)
findLetter 'D'  = (35, 16)
findLetter 'E'  = (36, 12)
findLetter 'F'  = (37, 16)
findLetter 'G'  = (38, 12)
findLetter 'H'  = (39, 16)
findLetter 'I'  = (40, 12)
findLetter 'J'  = (41, 12)
findLetter 'K'  = (42, 16)
findLetter 'L'  = (43, 16)
findLetter 'M'  = (44, 20)
findLetter 'N'  = (45, 16)
findLetter 'O'  = (46, 12)
findLetter 'P'  = (47, 16)
findLetter 'Q'  = (48, 12)
findLetter 'R'  = (49, 16)
findLetter 'S'  = (50, 12)
findLetter 'T'  = (51, 12)
findLetter 'U'  = (52, 16)
findLetter 'V'  = (53, 20)
findLetter 'W'  = (54, 16)
findLetter 'X'  = (55, 16)
findLetter 'Y'  = (56, 16)
findLetter 'Z'  = (57, 16)
findLetter '.'  = (58,  8)
findLetter ':'  = (59,  8)
findLetter ','  = (60,  8)
findLetter ';'  = (61,  8)
findLetter '+'  = (62, 12)
findLetter '-'  = (63, 12)
findLetter '*'  = (64, 12)
findLetter '/'  = (65, 12)
findLetter '='  = (66, 12)
findLetter '1'  = (67, 12)
findLetter '2'  = (68, 12)
findLetter '3'  = (69, 12)
findLetter '4'  = (70, 12)
findLetter '5'  = (71, 12)
findLetter '6'  = (72, 12)
findLetter '7'  = (73, 12)
findLetter '8'  = (74, 12)
findLetter '9'  = (75, 12)
findLetter '0'  = (76, 12)
findLetter c    = (79,  12)
