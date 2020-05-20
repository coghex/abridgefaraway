module Anamnesis.Data where
-- data for the application monad
import qualified Control.Monad.Logger as Logger
import Artos.Queue
data AnamnExcept = AnamnExcept
       { code ∷ Maybe AnamnResult
       , msg  ∷ String
       } deriving (Show)
data AnamnResult = AnamnSuccess | AnamnError deriving (Show)
-- the env holds pointers to shared
-- memory, system specific static
-- data, and any configuration that
-- must not change for any reason
data Env = Env { envEventsChan ∷ Queue Event }
-- the state holds any mutable data
-- the app may require or aquire.
data State = State { logFunc ∷ Logger.Loc → Logger.LogSource → Logger.LogLevel → Logger.LogStr → IO () }
