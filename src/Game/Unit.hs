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

drawUnit :: State -> [GL.TextureObject] -> Unit -> IO ()
drawUnit state texs unit = do
  start <- getCurrentTime
  let n = 1000
  withTextures2D [(head texs)] $ drawUnitTile [(head texs)] unit
  end <- getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) :: Int
      delay = n*1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  drawUnit state ((tail texs)++(head texs)) unit


drawUnitTile :: [GL.TextureObject] -> Unit -> IO ()
drawUnitTile tex unit = do
  glLoadIdentity
  glTranslatef x y (-thiszoom)
  glColor3f 1.0 1.0 1.0
  drawSquare
  where (x, y)   = position unit
        thiszoom = thisZoom
