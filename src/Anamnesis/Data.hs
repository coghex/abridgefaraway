module Anamnesis.Data
  --( AnamnExcept(..), AnamnResult(..)
  ( AnamnResult(..)
  , Env(..), State(..), LoopControl(..)
  ) where
-- data for the application monad
import qualified Control.Monad.Logger as Logger
import Artos.Except
import Artos.Queue
-- exceptions are expicitly
-- for anamnesis code
--data AnamnExcept = AnamnExcept
--       { code ∷ Maybe AnamnResult
--       , msg  ∷ String
--       } deriving (Show, Eq)
---- possible action results
data AnamnResult = AnamnSuccess | AnamnError deriving (Show, Eq)
-- loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq
-- the env holds pointers to shared
-- memory, system specific static
-- data, and any configuration that
-- must not change for any reason
data Env = Env { envEventsChan ∷ Queue Event }
-- the state holds any mutable data
-- the app may require or aquire.
data State = State { currentStatus ∷ AExcept
                   , logFunc       ∷ Logger.Loc → Logger.LogSource → Logger.LogLevel → Logger.LogStr → IO () }
