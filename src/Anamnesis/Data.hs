{-# LANGUAGE Strict #-}
module Anamnesis.Data where
-- data for continuation monad
import qualified Control.Monad.Logger as Logger
import Artos.Except
import Artos.Queue
import Epiklesis.Data
import Paracletus.Data
import Anamnesis.Draw
import qualified Paracletus.Oblatum.GLFW as GLFW
-- possible results of anamnesis
-- specific utility actions
data AnamnResult = AnamnSuccess | AnamnError deriving (Show, Eq)
-- glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq
-- env should only hold pointers/references
data Env = Env { envEventsChan ∷ Queue Event }
-- state holds mutable data, and the
-- current status of the whole App
data State = State { status  ∷ AExcept
                   , logFunc ∷ Logger.Loc → Logger.LogSource → Logger.LogLevel → Logger.LogStr → IO ()
                   , window  ∷ !(Maybe GLFW.Window)
                   , cam3d   ∷ !(Float, Float, Float)
                   , cursor  ∷ !(Int, Int, Int)
                   , stateChanged ∷ !Bool
                   , drawSt  ∷ !DrawState
                   , luaSt   ∷ !LuaState }
