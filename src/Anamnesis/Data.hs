{-# LANGUAGE Strict #-}
module Anamnesis.Data where
-- data for continuation monad
import Data.Time.Clock.System
import qualified Control.Monad.Logger as Logger
import Artos.Data
import Artos.Except
import Artos.Queue
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Oblatum.Data
import qualified Paracletus.Oblatum.GLFW as GLFW
-- possible results of anamnesis
-- specific utility actions
data AnamnResult = AnamnSuccess | AnamnError deriving (Show, Eq)
-- glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq
-- env should only hold pointers/references
data Env = Env { envEventsChan  ∷ Queue Event
               , envLCmdChan    ∷ Queue LoadCmd
               , envLTimerChan  ∷ TChan TState }
-- state holds mutable data, and the
-- current status of the whole App
data State = State { status       ∷ AExcept
                   , logFunc      ∷ Logger.Loc → Logger.LogSource → Logger.LogLevel → Logger.LogStr → IO ()
                   , windowSt     ∷ !(Maybe GLFW.Window)
                   , drawSt       ∷ !DrawState
                   , luaSt        ∷ !LuaState
                   , inputState   ∷ !InputState
                   , sVertCache   ∷ !(Maybe Verts)
                   , sTest        ∷ Int
                   , sReload      ∷ Bool
                   , sRecreate    ∷ Bool
                   , sStartTime   ∷ SystemTime
                   , sFPS         ∷ (Double,Int)
                   , sTick        ∷ Maybe Double }
data Settings = Settings
  { settingScreenW   ∷ Int
  , settingScreenH   ∷ Int
  , settingFontPath  ∷ FilePath
  , settingTBPath    ∷ FilePath
  , settingMTBPath    ∷ FilePath
  , settingTexPath   ∷ FilePath
  , settingKeyLayout ∷ GLFW.KeyLayout } deriving (Show, Eq)


