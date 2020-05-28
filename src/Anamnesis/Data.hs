{-# LANGUAGE Strict #-}
module Anamnesis.Data where
-- data for continuation monad
import qualified Control.Monad.Logger as Logger
import Artos.Except
import Artos.Queue
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
                   , logFunc ∷ Logger.Loc → Logger.LogSource → Logger.LogLevel → Logger.LogStr → IO () }
