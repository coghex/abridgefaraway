{-# LANGUAGE Strict #-}
module Anamnesis.Data where
-- data for continuation monad
import qualified Control.Monad.Logger as Logger
import Artos.Data
import Artos.Except
import Artos.Queue
import Epiklesis.Data
import Epiklesis.World
import Paracletus.Data
import Paracletus.Oblatum.Data
import Anamnesis.Draw
import qualified Paracletus.Oblatum.GLFW as GLFW
-- possible results of anamnesis
-- specific utility actions
data AnamnResult = AnamnSuccess | AnamnError deriving (Show, Eq)
-- glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq
-- env should only hold pointers/references
data Env = Env { envEventsChan ∷ Queue Event
               , envSCChan     ∷ TChan ((Float,Float),(Int,Int))
               , envSegChan    ∷ TChan [Segment]
               , envWTimerChan ∷ TChan TState }
-- state holds mutable data, and the
-- current status of the whole App
data State = State { status       ∷ AExcept
                   , logFunc      ∷ Logger.Loc → Logger.LogSource → Logger.LogLevel → Logger.LogStr → IO ()
                   , windowSt     ∷ !(Maybe GLFW.Window)
                   , cam3d        ∷ !(Float, Float, Float)
                   , gamecam3d    ∷ !(Float, Float, Float)
                   , cursor       ∷ !(Int, Int, Int)
                   , screenCursor ∷ !((Float, Float), (Int, Int))
                   , currentWin   ∷ Int
                   , drawSt       ∷ !DrawState
                   , luaSt        ∷ !LuaState
                   , inputState   ∷ !InputState
                   , sSettings    ∷ !Settings
                   , sRecreate    ∷ Bool }
data Settings = Settings
  { settingScreenW   ∷ Int
  , settingScreenH   ∷ Int
  , settingFontPath  ∷ FilePath
  , settingTBPath    ∷ FilePath
  , settingMTBPath    ∷ FilePath
  , settingTexPath   ∷ FilePath
  , settingKeyLayout ∷ GLFW.KeyLayout } deriving (Show, Eq)
