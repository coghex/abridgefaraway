module Game.State where

import Control.Concurrent.STM (TQueue)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.FTGL as FTGL

import System.Random
import Game.Sun
import Game.Moon
import Game.Data

data GameState  = SWorld | SElev | SSeaTemp | SSeaCurrents | SSkyTemp | SWind | SRain | SZone | SZoneElev | SMenu | SLoad | SLoadTime | SLoadZone | SLoadElev | SLoadZoneElev | SLoadSeaTemp | SLoadSeaCurrents | SLoadSkyTemp | SLoadWind | SLoadRain | SFucked | SPause deriving (Eq, Show)
data TimerState = TStart | TStop | TPause

data Env = Env
  { envEventsChan       :: TQueue Event
  , envWindow           :: !GLFW.Window
  , envFontBig          :: !FTGL.Font
  , envFontSmall        :: !FTGL.Font
  , envWTex             :: ![GL.TextureObject]
  , envZTex             :: ![[GL.TextureObject]]
  , envUTex             :: ![[GL.TextureObject]]
  , envUnitTex          :: ![[GL.TextureObject]]
  , envSeeds            :: ![Int]
  , envStateChan1       :: TChan State
  , envStateChan2       :: TChan State
  , envTimerChan        :: TChan TimerState
  , envUnitChan1        :: TChan [Unit]
  , envUnitChan2        :: TChan [Unit]
  , envUTimerChan       :: TChan TimerState
  }

data State = State
  { stateGame           :: !GameState
  , stateStdGens        :: ![StdGen]
  , stateScreenW        :: !Int
  , stateScreenH        :: !Int
  , stateZoom           :: !Float
  , stateGrid           :: ![Int]
  , stateOG             :: ![Int]
  , stateElev           :: ![Int]
  , stateCursor         :: !(Int, Int)
  , stateNConts         :: !Int
  , stateCurrMap        :: !Int
  , stateConts          :: ![(Int, Int)]
  , stateSeeds          :: ![[(Int, Int)]]
  , stateRands          :: ![[(Int, Int)]]
  , stateSizes          :: ![Int]
  , stateTypes          :: ![Int]
  , stateRandI          :: !Int
  , stateRangeRands     :: ![Int]
  , stateSun            :: !Sun
  , stateMoon           :: !Moon
  , stateSunSpots       :: ![Float]
  , stateTime           :: !Integer
  , stateOceans         :: ![Ocean]
  , stateOceanTempZ     :: !Int
  , stateOceanCurrentsZ :: !Int
  , stateSkies          :: ![Sky]
  , stateSkyTempZ       :: !Int
  , stateWindZ          :: !Int
  , stateZones          :: ![Zone]
  , stateUnits          :: ![Unit]
  } deriving (Show)

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
  | EventUpdateState     !State
  | EventUpdateUnits     ![Unit]
  deriving Show

