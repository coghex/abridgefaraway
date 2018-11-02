{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Game.FontRenderer (
  module T,
  initTextRenderer,
  TextResource(..)
) where

import Graphics.Rendering.OpenGL hiding (Bitmap)
import Graphics.GL
import Codec.Picture
import Control.Monad
import Data.Maybe
import Data.Char
import Data.Vector.Storable  (unsafeWith, Storable)
import System.IO             (hPutStrLn, stderr)
import System.Exit           (exitFailure)
import Data.Vector.Storable  (unsafeWith)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable      (sizeOf)
import Foreign.Ptr           (nullPtr)
import qualified Data.ByteString as B
import Game.Font
import Game.FontData as T

data TextResource = Font FilePath | Bitmap FilePath

vertSrc :: B.ByteString
vertSrc = B.intercalate "\n"
  [ "attribute vec2 position;"
  , "attribute vec2 uv;"
  , "varying vec2 vTex;"
  , "uniform mat4 mat4 modelview;"
  , "void main () {"
  , "    vTex = uv;"
  , "    gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
  , "}" ]

fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
  [ "varying vec2 vTex;"
  , "uniform sampler2D sampler;"
  , "uniform vec4 color;"
  , "void main() {"
  , "    vec4 tc = texture2D (sampler, vec2(vTex.s, vTex.t));"
  , "    gl_FragColor = tc;//vec4(tc.r*color.r,tc.g*color.g,tc.b*color.b,tc.a*color.a);"
  , "}" ]

vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr

uvDescriptor :: VertexArrayDescriptor [Float]
uvDescriptor = vertDescriptor

initTextRenderer :: TextResource -> IO TextRenderer
initTextRenderer rsrc = do
  v <- makeShader VertexShader vertSrc
  f <- makeShader FragmentShader fragSrc

  p <- makeProgram [v,f] [("position", AttribLocation 0), ("uv", AttribLocation 1)]
  currentProgram $= Just p
  UniformLocation mv <- get $ uniformLocation p "modelview"
  UniformLocation pj <- get $ uniformLocation p "projection"
  let updateMV mat = withArray mat $ \ptr -> glUniformMatrix4fv mv 1 1 ptr
      updatePJ mat = withArray mat $ \ptr -> glUniformMatrix4fv pj 1 1 ptr
  sLoc <- get $ uniformLocation p "sampler"
  cLoc <- get $ uniformLocation p "color"
  let updateSampler s = uniform sLoc $= s
      updateColor   c = uniform cLoc $= c
  -- load the font atlas
  t <- case rsrc of
    Font font  -> loadCharacter font 'A' 256 0
    Bitmap bmp -> do Just t <- initFontTexture bmp 0
                     return t
  let drawText _ = do 
                     let verts = [ 0, 0
                                 , 1, 0
                                 , 1, 1
                                 , 0, 0
                                 , 1, 1
                                 , 0, 1
                                 ] :: [GLfloat]
                         uvs      = verts
                         size     = length verts * sizeOf (undefined :: Float)
                     currentProgram $= Just p
                     [i,j] <- genObjectNames 2
                     -- buffer the verts
                     bindVBO i vertDescriptor $ AttribLocation 0
                     withArray verts $ \ptr ->
                       bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)
                     -- buffer the uvs
                     bindVBO j uvDescriptor $ AttribLocation 1
                     withArray uvs $ \ptr ->
                       bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)

                     texture Texture2D        $= Enabled
                     activeTexture            $= TextureUnit 0
                     textureBinding Texture2D $= Just t
                     bindVBO i vertDescriptor $ AttribLocation 0
                     bindVBO j uvDescriptor   $ AttribLocation 1
                     drawArrays Triangles 0 6
                     bindBuffer ArrayBuffer $= Nothing
                     deleteObjectNames [i,j]
  return TextRenderer { _textProgram    = RndrProgram3D { _program = p
                                                        , _setModelview = updateMV
                                                        , _setProjection = updatePJ
                                                        }
                      , _setSampler     = updateSampler
                      , _setTextColor   = updateColor
                      , _drawText       = drawText
                      }

makeShader :: ShaderType -> B.ByteString -> IO Shader
makeShader ty src = do
  s <- createShader ty
  shaderSourceBS s $= src
  compileShader s
  s'Ok <- get $ compileStatus s
  unless s'Ok $ do
    slog <- get $ shaderInfoLog s
    putStrLn $ "Shader Log:" ++ slog
    exitFailure
  return s

makeProgram :: [Shader] -> [(String, AttribLocation)] -> IO Program
makeProgram shaders attributes = do
  p <- createProgram
  mapM_ (attachShader p) shaders
  mapM_ (\(name, loc) -> attribLocation p name $= loc) attributes
  linkProgram p
  p'Ok <- get $ linkStatus p
  validateProgram p
  status <- get $ validateStatus p
  unless (p'Ok && status) $ do
    plog <- get $ programInfoLog p
    putStrLn plog
    exitFailure
  return p

bindVBO :: BufferObject -> VertexArrayDescriptor a -> AttribLocation -> IO ()
bindVBO vbo dsc loc = do
  bindBuffer ArrayBuffer  $= Just vbo
  vertexAttribPointer loc $= (ToFloat, dsc)
  vertexAttribArray   loc $= Enabled

initFontTexture :: FilePath -> Int -> IO (Maybe TextureObject)
initFontTexture file u = do
  texture Texture2D $= Enabled
  mTex <- loadFontTexture file u
  unless (isNothing mTex) $ do
    textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    textureWrapMode Texture2D T $= (Repeated, Clamp)
  when (isNothing mTex) $ putStrLn $ "Could not initialize "++ file
  return mTex

loadFontTexture :: FilePath -> Int -> IO (Maybe TextureObject)
loadFontTexture f u = do
  eDynImg <- readImage f
  case eDynImg of
    Left note -> do
      putStrLn $ "Could not load texture '"++f++"'.\nNote: "++note
      return Nothing
    Right img -> do
      tex <- newBoundFontTexUnit u
      success <- bufferDataIntoBoundFontTex img
      unless success $ putStrLn $ "    ("++f++")"
      return $ Just tex

newBoundFontTexUnit :: Int -> IO TextureObject
newBoundFontTexUnit u = do
  [tex] <- genObjectNames 1
  texture Texture2D $= Enabled
  activeTexture     $= TextureUnit (fromIntegral u)
  textureBinding Texture2D $= Just tex
  return tex

bufferDataIntoBoundFontTex :: DynamicImage -> IO Bool
bufferDataIntoBoundFontTex dynImg = case dynImg of
  (ImageRGB8  img) -> unsafeFontTexImage2D RGB8  RGB  img
  (ImageRGBA8 img) -> unsafeFontTexImage2D RGBA8 RGBA img
  _                -> do
    putStrLn "font texture not in expected format"
    return False

unsafeFontTexImage2D :: (Storable t1, PixelBaseComponent t ~ t1) => PixelInternalFormat -> PixelFormat -> Image t -> IO Bool
unsafeFontTexImage2D rb r (Image w h dat) = do
  unsafeWith dat $ \ptr ->
    texImage2D Texture2D NoProxy 0 rb (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (PixelData r UnsignedByte ptr)
  return True

















