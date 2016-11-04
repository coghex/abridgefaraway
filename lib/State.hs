module State where

import Control.Concurrent.STM (TQueue)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

data GameState = SWorld | SMenu

data Env = Env
    { envEventsChan   :: TQueue Event
    , envWindow       :: !GLFW.Window
    , envGridWidth    :: !Int
    , envGridHeight   :: !Int
    }

data State = State
    { stateGrid       :: ![Int]
    , stateTexs       :: ![GL.GLuint]
    , stateGame       :: !GameState
    , stateConts      :: ![(Int, Int)]
    , stateSeeds      :: ![[(Int, Int)]]
    , stateRands      :: ![[(Int, Int)]]
    , stateTileSizes  :: ![Int]
    , stateTileRands  :: ![Int]
    , stateContSizes  :: ![(Int, Int)]
    , stateSIceSizes  :: ![(Int, Int)]
    , stateSIces      :: ![(Int, Int)]
    , stateSIceRands  :: ![(Int, Int)]
    , stateNIceSizes  :: ![(Int, Int)]
    , stateNIces      :: ![(Int, Int)]
    , stateNIceRands  :: ![(Int, Int)]
    , stateZazzs      :: ![(Int, Int)]
    , stateZazzSizes  :: ![(Int, Int)]
    , stateZazzRands  :: ![(Int, Int)]
    }

data Event =
      EventError !GLFW.Error  !String
    | EventKey   !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
