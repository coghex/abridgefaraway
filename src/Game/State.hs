module Game.State where

import Control.Concurrent.STM (TQueue)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.FTGL as FTGL
import System.Random

data GameState = SWorld | SZone | SMenu | SLoad | SFucked deriving (Eq)

data Env = Env
    { envEventsChan   :: TQueue Event
    , envWindow       :: !GLFW.Window
    , envFontBig      :: !FTGL.Font
    , envFontSmall    :: !FTGL.Font
    , envWTexs        :: ![GL.TextureObject]
    , envZTexs        :: ![[GL.TextureObject]]
    , envSeeds        :: ![StdGen]
    }

data State = State
    { stateGame       :: !GameState
    , stateScreenW    :: !Int
    , stateScreenH    :: !Int
    , stateGrid       :: ![Int]
    , stateCursor     :: !(Int, Int)
    , stateNConts     :: !Int
    , stateISpots     :: !Int
    , stateZSpots     :: !Int
    , stateNSpots     :: ![Int]
    , stateConts      :: ![(Int, Int)]
    , stateSeeds      :: ![[(Int, Int)]]
    , stateRands      :: ![[(Int, Int)]]
    , stateSizes      :: ![Int]
    , stateTypes      :: ![Int]
    , stateSIceConts  :: ![(Int, Int)]
    , stateSIceSizes  :: ![(Int, Int)]
    , stateSIceRands  :: ![(Int, Int)]
    , stateNIceConts  :: ![(Int, Int)]
    , stateNIceSizes  :: ![(Int, Int)]
    , stateNIceRands  :: ![(Int, Int)]
    , stateZazzConts  :: ![(Int, Int)]
    , stateZazzSizes  :: ![(Int, Int)]
    , stateZazzRands  :: ![(Int, Int)]
    , stateZazzTypes  :: ![Int]
    , stateZone       :: ![Int]
    , stateZoneCamx   :: !GL.GLfloat
    , stateZoneCamy   :: !GL.GLfloat
    , stateZoneCamz   :: !GL.GLfloat
    , stateCurrentZ   :: ![Int]
    , stateBushes     :: ![(Int, Int)]
    , stateBRands     :: ![(Int, Int)]
    , stateBSizes     :: ![Int]
    , statePaths      :: ![Int]
    , statePathRands  :: ![Int]
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