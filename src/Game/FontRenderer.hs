{-# LANGUAGE OverloadedStrings #-}
module Game.FontRenderer (
  module T,
  initTextRenderer,
  TextResource(..)
) where

import Graphics.Types as T
import Graphics.Rendering.OpenGL hiding (Bitmap)
import Graphics.Rendering.OpenGL.Raw
import Codec.Picture
import Control.Monad
import Data.Maybe
import System.Exit           (exitFailure)
import Data.Vector.Storable  (unsafeWith)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable      (sizeOf)
import Foreign.Ptr           (nullPtr)
import qualified Data.ByteString as B

data TextResource Font FilePath | Bitmap FilePath

vertSrc :: B.ByteString
vertSrc = B.intercalate "\n"
  [ "attribute vec2 position;"
  , "attribute vec2 uv;"
