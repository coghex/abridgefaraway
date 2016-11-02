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
    , stateXs         :: ![Int]
    , stateYs         :: ![Int]
    , stateXSizes     :: ![Int]
    , stateYSizes     :: ![Int]
    , stateXRands     :: ![Int]
    , stateYRands     :: ![Int]
    , stateSeeds      :: ![Int]
    }

data Event =
      EventError !GLFW.Error  !String
    | EventKey   !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
