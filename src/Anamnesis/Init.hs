module Anamnesis.Init where
-- Anamnesis is initialized
import Control.Monad.Logger as Logger
import Data.IORef (IORef, newIORef)
import Anamnesis
import Anamnesis.Data
import Artos.Except
import Artos.Queue
import Artos.Var
-- returns a tuple style status
runAnamnesis ∷ (Either AExcept a → IO r) → Anamnesis r e s a → IO r
runAnamnesis c p = do
  res ← initRes
  env ← initEnv
  st  ← initState
  unAnamnate p res env st c
initEnv ∷ IO (TVar Env)
initEnv = do
  newQ ← newQueue
  atomically $ newTVar Env { envEventsChan = newQ }
initState ∷ IO (TVar State)
initState = do
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  atomically $ newTVar State { logFunc = lf }
initRes ∷ IO (IORef AExcept)
initRes = newIORef $ AExcept (Just AnamnSuccess) "" ""
