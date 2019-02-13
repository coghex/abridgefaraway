module Game.Unit where

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import System.Random (StdGen, mkStdGen)
import qualified Graphics.UI.GLFW as GLFW
import GLUtil.Textures
import Data.Time.Clock
import Control.Concurrent

import Game.Settings
import Game.State
import Game.Data
import Game.Draw
import Game.Map

drawUnits :: State -> IO ()
drawUnits state = resequence_ (map (drawUnit state) units)
  where units = stateUnits state

drawUnit :: State -> Unit -> IO ()
drawUnit state unit = do
  let texs = unittexs unit
  withTextures2D [(head texs)] $ drawUnitTile [(head texs)] unit

animateUnits :: State -> [Unit]
animateUnits state = map animateUnit units
  where units = stateUnits state

animateUnit :: Unit -> Unit
animateUnit u0 = Unit { unittexs = frameControl texs uf
                      , frame    = frameCounter uf
                      , unittype = ut
                      , zone     = uz
                      , position = up }
  where texs = unittexs u0
        uf   = frame u0
        ut   = unittype u0
        uz   = zone u0
        up   = position u0

frameControl :: [GL.TextureObject] -> Int -> [GL.TextureObject]
frameControl texs frame
  | frame >= 1 = ((tail texs) ++ [(head texs)])
  | otherwise   = texs

frameCounter :: Int -> Int
frameCounter frame
  | frame >= 2 = 0
  | otherwise   = frame+1

drawUnitTile :: [GL.TextureObject] -> Unit -> IO ()
drawUnitTile tex unit = do
  glLoadIdentity
  glTranslatef x y (-zoom/4)
  glColor3f 1.0 1.0 1.0
  drawSquare
  where (x, y)   = position unit
        thiszoom = fromIntegral theZoom
