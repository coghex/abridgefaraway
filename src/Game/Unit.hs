module Game.Unit where

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import System.Random (StdGen, mkStdGen)
import qualified Graphics.UI.GLFW as GLFW
import GLUtil.Textures

import Game.Settings
import Game.Draw

--drawUnit :: State -> [GL.TextureObject] -> Int -> IO ()
--drawUnit state texs unit = do\
--  withTextures2D :
