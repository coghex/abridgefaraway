module State where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL

data WorldArgs = WorldArgs { posArgs :: [(Int, Int)]
                           , ranArgs :: [[(Int, Int)]]
                           , sizArgs :: [(Int, Int)]
                           , themap :: [Int]
                           , win :: GLFW.Window
                           , texs :: [GLuint]
                           }

data GameState = SMenu | SWorld
type GameStateNow = GameState

packArgs :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> GLFW.Window -> [GLuint] -> WorldArgs
packArgs a b c d e f = WorldArgs (zip a b) [(zip c d)] (zip c d) e f
