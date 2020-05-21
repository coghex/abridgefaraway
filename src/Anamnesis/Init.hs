{-# LANGUAGE FlexibleContexts #-}
module Anamnesis.Init where
-- Anamnesis is initialized
import Control.Concurrent
import Control.Monad.Logger as Logger
import Data.IORef (IORef, newIORef, readIORef)
import System.Exit
import Anamnesis
import Anamnesis.Data
import Artos
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
-- forks a new instance 
occupyThreadAndFork ∷ Anamnesis r e s () → Anamnesis' e s () → Anamnesis r e s ()
occupyThreadAndFork mainProg deputyProg = Anamnesis $ \ref env st c → do
  mainThreadId ← myThreadId
  threadRef ← newIORef =<< readIORef ref
  _ ← Control.Concurrent.forkFinally (unAnamnate deputyProg threadRef env st pure >>= checkStatus) $ \case
    Left exception → throwTo mainThreadId exception
    Right ()       → throwTo mainThreadId ExitSuccess
  unAnamnate mainProg ref env st c
