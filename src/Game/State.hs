module Game.State where

import Control.Concurrent.STM (TQueue)
import Control.Concurrent.Chan
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.FTGL as FTGL

import System.Random
import Game.Sun

data GameState = SWorld | SElev | SZone | SMenu | SLoad | SLoadElev | SFucked | SPause deriving (Eq, Show)

data Env = Env
  { envEventsChan   :: TQueue Event
  , envWindow       :: !GLFW.Window
  , envFontBig      :: !FTGL.Font
  , envFontSmall    :: !FTGL.Font
  , envWTex         :: ![GL.TextureObject]
  , envZTex         :: ![[GL.TextureObject]]
  , envSeeds        :: ![Int]
  , envTimeChan     :: Chan Integer
  , envSunChan      :: Chan Sun
  }

data State = State
  { stateGame       :: !GameState
  , stateStdGens    :: ![StdGen]
  , stateScreenW    :: !Int
  , stateScreenH    :: !Int
  , stateGrid       :: ![Int]
  , stateElev       :: ![Int]
  , stateCursor     :: !(Int, Int)
  , stateNConts     :: !Int
  , stateCurrMap    :: !Int
  , stateConts      :: ![(Int, Int)]
  , stateSeeds      :: ![[(Int, Int)]]
  , stateRands      :: ![[(Int, Int)]]
  , stateSizes      :: ![Int]
  , stateTypes      :: ![Int]
  , stateRandI      :: !Int
  , stateRangeRands :: ![Int]
  , stateSun        :: !Sun
  , stateSunSpots   :: ![Float]
  , stateTime       :: !Integer
  }

data Event =
    EventError           !GLFW.Error  !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  | EventWindowResize    !GLFW.Window !Int !Int
  | EventLoaded          !GameState
  | EventUpdateTime      !Integer !Sun
  deriving Show

