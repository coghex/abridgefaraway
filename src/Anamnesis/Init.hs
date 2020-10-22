{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
module Anamnesis.Init where
-- init functions can be found
import Prelude()
import UPrelude
import Control.Concurrent
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Logger.CallStack as LoggerCS
import Anamnesis
import Anamnesis.Data
import Anamnesis.Draw
import Paracletus.Data
import Artos
import Artos.Except
import Artos.Queue
import Artos.Var
import Epiklesis.Lua

runAnamnesis ∷ (Either AExcept α → IO σ) → Anamnesis ε σ α → IO σ
runAnamnesis c p = do
  env ← initEnv
  st  ← initState
  unAnamnate p env st c
initEnv ∷ IO (TVar Env)
initEnv = do
  newQ ← newQueue
  atomically $ newTVar Env { envEventsChan = newQ }
initState ∷ IO (TVar State)
initState = do
  let ref = AExcept (Just AnamnSuccess) ExAnamnesis ""
  let tile1 = GTile { tPos   = (0,0)
                    , tScale = (10,10)
                    , tInd   = (0,0)
                    , tSize  = (1,1)
                    , tT     = 0 }
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  ls ← initLua
  luasettings ← importSettings ls "mod/base/"
  ds ← initDrawState [tile1]
  atomically $ newTVar State { status       = ref
                             , logFunc      = lf
                             , windowSt     = Nothing
                             , cam3d        = (2.0, 2.0, 2.0)
                             , cursor       = (0, 0, 2)
                             , currentWin   = 0
                             , drawSt       = ds
                             , luaSt        = ls
                             , sSettings    = luasettings
                             , sRecreate    = False }
