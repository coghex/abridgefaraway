module State where

import Control.Concurrent.STM (TQueue)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

data GameState = SWorld | SZone | SMenu | SLoad deriving (Eq)

data Env = Env
    { envEventsChan   :: TQueue Event
    , envWindow       :: !GLFW.Window
    , envGridWidth    :: !Int
    , envGridHeight   :: !Int
    }

data State = State
    { stateGrid       :: ![Int]
    , stateScreenWidth  :: !Int
    , stateScreenHeight :: !Int
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
    , stateZazzTypes  :: ![Int]
    , stateCursor     :: !Bool
    , stateCursorX    :: !Int
    , stateCursorY    :: !Int
    , stateAlphabet   :: ![GL.TextureObject]
    }

data Event =
    EventError           !GLFW.Error  !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  | EventWindowResize    !GLFW.Window !Int !Int
  | EventLoaded
  deriving Show
