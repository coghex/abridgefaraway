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
animateUnit u0 = Unit { unittexs = ((tail texs) ++ [(head texs)])
                      , unittype = ut
                      , zone     = uz
                      , position = up }
  where texs = unittexs u0
        ut   = unittype u0
        uz   = zone u0
        up   = position u0

drawUnitTile :: [GL.TextureObject] -> Unit -> IO ()
drawUnitTile tex unit = do
  glLoadIdentity
  glTranslatef x y (-thiszoom)
  glColor3f 1.0 1.0 1.0
  drawSquare
  where (x, y)   = position unit
        thiszoom = fromIntegral theZoom
