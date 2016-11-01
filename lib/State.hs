module State where

import Control.Concurrent.STM (TQueue)
import Graphics.UI.GLFW as GLFW

data Env = Env
  { envEventsChan :: TQueue Event
  , envWindow     :: !GLFW.Window
  }

data GameS = SWorld | SMenu

data State = State
  { stateWindowWidth  :: !Int
  , stateWindowHeight :: !Int
  , stateMouseDown    :: !Bool
  , stateDragging     :: !Bool
  , stateDragStartX   :: !Double
  , stateDragStartY   :: !Double
  , stateMap          :: ![Int]
  , stateGame         :: !GameS
  , stateXs           :: [Int]
  , stateYs           :: [Int]
  , stateXSizes       :: [Int]
  , stateYSizes       :: [Int]
  , stateXRands       :: [Int]
  , stateYRands       :: [Int]
  }

data Event =
    EventError           !GLFW.Error !String
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
  deriving Show
