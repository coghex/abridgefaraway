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

data FontType = NULLFONT | SMFONT | CQFONT deriving (Show, Eq)
data FontAttribute = FBOLD | FRED | FYELLOW | FBLUE | FITALIC | FNULL deriving (Show, Eq)
data Font = Font { fontfilepath :: String
                 , fonttype     :: FontType
                 , texs         :: [GL.TextureObject] } deriving (Show, Eq)


newFont :: String -> FontType -> [GL.TextureObject] -> Font
newFont fp fptype texs = Font { fontfilepath = fp
                       , fonttype     = fptype
                       , texs         = texs }

nullFont :: Font
nullFont        = Font { fontfilepath = ""
                       , fonttype     = NULLFONT
                       , texs         = [] }

makeFonts :: [[GL.TextureObject]] -> [Font]
makeFonts texs = [nullFont, (newFont "data/fonts/sm/smalph" SMFONT (texs!!1)), (newFont "data/fonts/cq/smalph" CQFONT (texs!!2))]

--loadFont :: String -> IO (Font)
--loadFont fp = do
--  texs <- loadNTexs 80 fpfull []
--  return $ newFont fpfull ft texs
--  where fpfull = "data/fonts/" ++ fp
--        ft     = 

loadNTexs :: Int -> String -> [GL.TextureObject] -> IO ([GL.TextureObject])
loadNTexs 0 _  texs = return texs
loadNTexs n fp texs = do
  x <- loadTex fpfull
  loadNTexs (n-1) fp (x : texs)
  where fpfull = fp ++ (show (n-1)) ++ ".png"

loadFontTextures :: String -> IO ([[GL.TextureObject]])
loadFontTextures str = do
  smfont <- loadNTexs 80 smfp []
  cqfont <- loadNTexs 80 cqfp []
  return $ [[], smfont, cqfont]
  where smfp = str ++ "sm/smalph"
        cqfp = str ++ "cq/smalph"

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
  
drawFont :: Font -> Int -> FontAttribute -> (Float, Float) -> String -> IO ()
drawFont font size attr pos str = drawFontLoop font size attr pos str

drawFontLoop :: Font -> Int -> FontAttribute -> (Float, Float) -> String -> IO ()
drawFontLoop _    _    _    (_, _) []      = glFlush
drawFontLoop font size attr (x, y) (l:str) = do
  withTextures2D [(ts!!n)] $ drawFontSquare ts attr l (fromIntegral size) x y
  drawFontLoop font size attr ((x+(nstep)), y) str
  where ts         = texs font
        (n, nsize) = findLetter ft l
        nstep      = (fromIntegral nsize) / 32.0
        ft         = fonttype font

drawFontSquare :: [GL.TextureObject] -> FontAttribute -> Char -> Float -> Float -> Float -> IO ()
drawFontSquare texs FYELLOW c zoom x y = do
  glLoadIdentity
  glTranslatef (2*x) (2*y) (-zoom)
  glColor3f 0.9 1.0 0.4
  drawSquare
drawFontSquare texs attr    c zoom x y = do
  glLoadIdentity
  glTranslatef (2*x) (2*y) (-zoom)
  glColor3f 1.0 1.0 1.0
  drawSquare

-- will find which number the current char maps to, and the size of that letter, depending on the font
findLetter :: FontType -> Char -> (Int, Int)
findLetter SMFONT 'a'  = ( 0, 12)
findLetter SMFONT 'b'  = ( 1, 12)
findLetter SMFONT 'c'  = ( 2, 12)
findLetter SMFONT 'd'  = ( 3, 12)
findLetter SMFONT 'e'  = ( 4, 12)
findLetter SMFONT 'f'  = ( 5, 12)
findLetter SMFONT 'g'  = ( 6, 12)
findLetter SMFONT 'h'  = ( 7, 12)
findLetter SMFONT 'i'  = ( 8,  8)
findLetter SMFONT 'j'  = ( 9,  8)
findLetter SMFONT 'k'  = (10, 12)
findLetter SMFONT 'l'  = (11,  8)
findLetter SMFONT 'm'  = (12, 16)
findLetter SMFONT 'n'  = (13, 12)
findLetter SMFONT 'o'  = (14, 12)
findLetter SMFONT 'p'  = (15, 12)
findLetter SMFONT 'q'  = (16, 12)
findLetter SMFONT 'r'  = (17, 12)
findLetter SMFONT 's'  = (18, 12)
findLetter SMFONT 't'  = (19, 12)
findLetter SMFONT 'u'  = (20, 12)
findLetter SMFONT 'v'  = (21, 12)
findLetter SMFONT 'w'  = (22, 16)
findLetter SMFONT 'x'  = (23, 12)
findLetter SMFONT 'y'  = (24, 12)
findLetter SMFONT 'z'  = (25, 12)
findLetter SMFONT '?'  = (26, 12)
findLetter SMFONT '!'  = (27,  8)
findLetter SMFONT '('  = (28,  8)
findLetter SMFONT ')'  = (29,  8)
findLetter SMFONT '\'' = (30,  8)
findLetter SMFONT '"'  = (31,  8)
findLetter SMFONT 'A'  = (32, 16)
findLetter SMFONT 'B'  = (33, 16)
findLetter SMFONT 'C'  = (34, 12)
findLetter SMFONT 'D'  = (35, 16)
findLetter SMFONT 'E'  = (36, 12)
findLetter SMFONT 'F'  = (37, 16)
findLetter SMFONT 'G'  = (38, 12)
findLetter SMFONT 'H'  = (39, 16)
findLetter SMFONT 'I'  = (40, 12)
findLetter SMFONT 'J'  = (41, 12)
findLetter SMFONT 'K'  = (42, 16)
findLetter SMFONT 'L'  = (43, 16)
findLetter SMFONT 'M'  = (44, 20)
findLetter SMFONT 'N'  = (45, 16)
findLetter SMFONT 'O'  = (46, 12)
findLetter SMFONT 'P'  = (47, 16)
findLetter SMFONT 'Q'  = (48, 12)
findLetter SMFONT 'R'  = (49, 16)
findLetter SMFONT 'S'  = (50, 12)
findLetter SMFONT 'T'  = (51, 12)
findLetter SMFONT 'U'  = (52, 16)
findLetter SMFONT 'V'  = (53, 20)
findLetter SMFONT 'W'  = (54, 24)
findLetter SMFONT 'X'  = (55, 16)
findLetter SMFONT 'Y'  = (56, 16)
findLetter SMFONT 'Z'  = (57, 16)
findLetter SMFONT '.'  = (58,  8)
findLetter SMFONT ':'  = (59,  8)
findLetter SMFONT ','  = (60,  8)
findLetter SMFONT ';'  = (61,  8)
findLetter SMFONT '+'  = (62, 12)
findLetter SMFONT '-'  = (63, 12)
findLetter SMFONT '*'  = (64, 12)
findLetter SMFONT '/'  = (65, 12)
findLetter SMFONT '='  = (66, 12)
findLetter SMFONT '1'  = (67, 12)
findLetter SMFONT '2'  = (68, 12)
findLetter SMFONT '3'  = (69, 12)
findLetter SMFONT '4'  = (70, 12)
findLetter SMFONT '5'  = (71, 12)
findLetter SMFONT '6'  = (72, 12)
findLetter SMFONT '7'  = (73, 12)
findLetter SMFONT '8'  = (74, 12)
findLetter SMFONT '9'  = (75, 12)
findLetter SMFONT '0'  = (76, 12)
findLetter SMFONT '%'  = (77, 22)
findLetter SMFONT '&'  = (78, 16)
findLetter CQFONT 'a'  = ( 0, 30)
findLetter CQFONT 'b'  = ( 1, 24)
findLetter CQFONT 'c'  = ( 2, 26)
findLetter CQFONT 'd'  = ( 3, 26)
findLetter CQFONT 'e'  = ( 4, 20)
findLetter CQFONT 'f'  = ( 5, 20)
findLetter CQFONT 'g'  = ( 6, 28)
findLetter CQFONT 'h'  = ( 7, 28)
findLetter CQFONT 'i'  = ( 8, 14)
findLetter CQFONT 'j'  = ( 9, 20)
findLetter CQFONT 'k'  = (10, 28)
findLetter CQFONT 'l'  = (11, 20)
findLetter CQFONT 'm'  = (12, 32)
findLetter CQFONT 'n'  = (13, 26)
findLetter CQFONT 'o'  = (14, 30)
findLetter CQFONT 'p'  = (15, 22)
findLetter CQFONT 'q'  = (16, 32)
findLetter CQFONT 'r'  = (17, 24)
findLetter CQFONT 's'  = (18, 20)
findLetter CQFONT 't'  = (19, 22)
findLetter CQFONT 'u'  = (20, 28)
findLetter CQFONT 'v'  = (21, 26)
findLetter CQFONT 'w'  = (22, 32)
findLetter CQFONT 'x'  = (23, 26)
findLetter CQFONT 'y'  = (24, 28)
findLetter CQFONT 'z'  = (25, 22)
findLetter CQFONT '?'  = (26, 16)
findLetter CQFONT '!'  = (27, 12)
findLetter CQFONT '('  = (28, 12)
findLetter CQFONT ')'  = (29, 12)
findLetter CQFONT '\'' = (30,  8)
findLetter CQFONT '"'  = (31,  8)
findLetter CQFONT 'A'  = (32, 28)
findLetter CQFONT 'B'  = (33, 24)
findLetter CQFONT 'C'  = (34, 26)
findLetter CQFONT 'D'  = (35, 26)
findLetter CQFONT 'E'  = (36, 20)
findLetter CQFONT 'F'  = (37, 20)
findLetter CQFONT 'G'  = (38, 28)
findLetter CQFONT 'H'  = (39, 28)
findLetter CQFONT 'I'  = (40, 14)
findLetter CQFONT 'J'  = (41, 20)
findLetter CQFONT 'K'  = (42, 28)
findLetter CQFONT 'L'  = (43, 20)
findLetter CQFONT 'M'  = (44, 32)
findLetter CQFONT 'N'  = (45, 26)
findLetter CQFONT 'O'  = (46, 30)
findLetter CQFONT 'P'  = (47, 22)
findLetter CQFONT 'Q'  = (48, 32)
findLetter CQFONT 'R'  = (49, 24)
findLetter CQFONT 'S'  = (50, 20)
findLetter CQFONT 'T'  = (51, 22)
findLetter CQFONT 'U'  = (52, 28)
findLetter CQFONT 'V'  = (53, 26)
findLetter CQFONT 'W'  = (54, 32)
findLetter CQFONT 'X'  = (55, 26)
findLetter CQFONT 'Y'  = (56, 28)
findLetter CQFONT 'Z'  = (57, 22)
findLetter CQFONT '.'  = (58, 10)
findLetter CQFONT ':'  = (59, 10)
findLetter CQFONT ','  = (60, 10)
findLetter CQFONT ';'  = (61, 10)
findLetter CQFONT '+'  = (62, 20)
findLetter CQFONT '-'  = (63, 20)
findLetter CQFONT '*'  = (64, 16)
findLetter CQFONT '/'  = (65, 16)
findLetter CQFONT '='  = (66, 20)
findLetter CQFONT '1'  = (67, 14)
findLetter CQFONT '2'  = (68, 22)
findLetter CQFONT '3'  = (69, 18)
findLetter CQFONT '4'  = (70, 22)
findLetter CQFONT '5'  = (71, 20)
findLetter CQFONT '6'  = (72, 20)
findLetter CQFONT '7'  = (73, 20)
findLetter CQFONT '8'  = (74, 22)
findLetter CQFONT '9'  = (75, 20)
findLetter CQFONT '0'  = (76, 22)
findLetter CQFONT '%'  = (77, 26)
findLetter CQFONT '&'  = (78, 26)
findLetter SMFONT c    = (79, 12)
findLetter CQFONT c    = (79, 24)

-- returns the length of a string in a certain font
lengthOfString :: Font -> String -> Float
lengthOfString _    []      = 0
lengthOfString font (c:str) = ((fromIntegral (snd (findLetter ft c))) / 32.0) + (lengthOfString font str)
   where ft = fonttype font
