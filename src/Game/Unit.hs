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
import Game.Zone

drawUnits :: State -> IO ()
drawUnits state = resequence_ (map (drawUnit state) units)
  where units = stateUnits state

drawUnit :: State -> Unit -> IO ()
drawUnit state unit = do
  let texs = unittexs unit
      (camx, camy, _) = getZoneCam (head (stateZones state))
  withTextures2D [(head texs)] $ drawUnitTile [(head texs)] camx camy unit

animateUnits :: State -> [Unit]
animateUnits state = map animateUnit units
  where units = stateUnits state

animateUnit :: Unit -> Unit
animateUnit u0 = Unit { unittexs = frameControl len texs uf
                      , frame    = frameCounter len uf
                      , unittype = ut
                      , zone     = uz
                      , position = up }
  where texs = unittexs u0
        uf   = frame u0
        ut   = unittype u0
        uz   = zone u0
        up   = position u0
        len  = length texs - 1

frameControl :: Int -> [GL.TextureObject] -> Int -> [GL.TextureObject]
frameControl len texs frame
  | frame >= len = ((tail texs) ++ [(head texs)])
  | otherwise    = texs

frameCounter :: Int -> Int -> Int
frameCounter len frame
  | frame >= len = 0
  | otherwise    = frame+1

drawUnitTile :: [GL.TextureObject] -> Float -> Float -> Unit -> IO ()
drawUnitTile tex camx camy unit = do
  glLoadIdentity
  glTranslatef (2*((tx) - ((fromIntegral zonew)/2))) (2*((ty) - ((fromIntegral zoneh)/2))) (-zoom/4)
  glColor3f 1.0 1.0 1.0
  drawSquare
  where (x, y)   = position unit
        tx       = x + camx
        ty       = y + camy
        thiszoom = fromIntegral theZoom
