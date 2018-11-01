module Game.Font where

import Control.Monad
import Graphics.Rendering.OpenGL hiding (bitmap)
import Graphics.Rendering.FreeType.Internal
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize as BS
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.FaceType
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Foreign
import Foreign.C.String
import Graphics.Rendering.FreeType.Internal.Bitmap
import qualified Graphics.Rendering.FTGL as FTGL
import Game.Settings
import Game.Util

beginDrawText :: IO ()
beginDrawText = do
  sceneSetup
  blend                      $= Enabled
  blendFunc                  $= (SrcAlpha, OneMinusSrcAlpha)

loadFont :: String -> IO (FTGL.Font)
loadFont fn = FTGL.createTextureFont fn

drawText :: FTGL.Font -> Int -> Int -> Int -> Int -> String -> IO ()
drawText font x y sx sy str = do
  loadIdentity
  translate $ Vector3 (2*((fromIntegral x) - (fromIntegral 60))) (2*((fromIntegral y) - (fromIntegral 45))) (-500::GLfloat)
  color (Color3 1 1 1 :: Color3 GLfloat)
  FTGL.setFontFaceSize font (quot sx 2) (quot sy 2)
  FTGL.renderFont font str FTGL.Front

-- generates texture, from a text renderer

-- un monads IO with a freetype error
runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
  r <- m
  unless (r == 0) $ fail $ "FreeType error: " ++  show r

-- given FT_Library and filepath retyrn loaded font face as FT_Face
fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
  alloca $ \ptr -> do
    runFreeType $ ft_New_Face ft str 0 ptr
    peek ptr

-- gives a string from a glyph format
glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
  | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
  | fmt == ft_GLYPH_FORMAT_OUTLINE   = "ft_GLYPH_FORMAT_OUTLINE"
  | fmt == ft_GLYPH_FORMAT_PLOTTER   = "ft_GLYPH_FORMAT_PLOTTER"
  | fmt == ft_GLYPH_FORMAT_BITMAP    = "ft_GLYPH_FORMAT_BITMAP"
  | otherwise                        = "ft_GLYPH_FORMAT_NAME"

-- allocs a c poninter to FT_Library
freeType :: IO FT_Library
freeType = alloca $ \p -> do
  runFreeType $ ft_Init_FreeType p
  peek p

-- this will load a character from a font into a textureObject
loadCharacter :: FilePath -> Char -> Int -> Int -> IO TextureObject
loadCharacter path char px texunit = do
  ft <- freeType
  ff <- fontFace ft path
  runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0
  -- unicode char index
  chindex <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char
  -- load glyph into freetype memory
  runFreeType $ ft_Load_Glyph ff chindex 0
  -- get the glyph slot
  slot <- peek $ glyph ff
  -- number of glyphs
  n <- peek $ num_glyphs ff
  -- glyph format
  fmt <- peek $ format slot
  -- glyph sizes
  numSizes <- peek $ num_fixed_sizes ff
  sizesPtr <- peek $ available_sizes ff
  sizes <- forM [0..numSizes-1] $ \i ->
    peek $ sizesPtr `plusPtr` fromIntegral i :: IO BS.FT_Bitmap_Size
  -- bitmap border
  l <- peek $ bitmap_left slot
  t <- peek $ bitmap_top slot
  -- render glyph in freetype memory
  runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
  -- get the char bitmap
  bmp <- peek $ bitmap slot
  let w  = fromIntegral $ width bmp
      h  = fromIntegral $ rows bmp
  -- i dont know why this code does this:
      w' = fromIntegral w
      h' = fromIntegral h
  -- set texture params on bound tex
  texture Texture2D $= Enabled
  -- fix bmp border
  rowAlignment Unpack $= 1
  -- generate GL tex
  tex <- newBoundTexUnit texunit
  -- load GL tex
  texImage2D Texture2D NoProxy 0 R8 (TextureSize2D w' h') 0 (PixelData Red UnsignedByte $ buffer bmp)
  -- texture filter and wrap modes
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

  return tex

-- this will generate a new GL texture for loading characters
newBoundTexUnit :: Int -> IO TextureObject
newBoundTexUnit u = do
  [tex] <- genObjectNames 1
  texture Texture2D        $= Enabled
  activeTexture            $= TextureUnit (fromIntegral u)
  textureBinding Texture2D $= Just tex
  return tex









