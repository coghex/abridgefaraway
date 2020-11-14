{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
module Anamnesis.Init where
-- init functions can be found
import Prelude()
import UPrelude
import qualified Control.Monad.Logger as Logger
import Anamnesis
import Anamnesis.Data
import Anamnesis.Draw
import Paracletus.Data
import Paracletus.Oblatum.Data
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
  newQ  ← newQueue
  newC1 ← newTChan
  newC2 ← newTChan
  newC3 ← newTChan
  newC4 ← newTChan
  newC5 ← newTChan
  newC6 ← newTChan
  atomically $ newTVar Env { envEventsChan  = newQ
                           , envCamChan     = newC1
                           , envCBChan      = newC2
                           , envCBDChan     = newC3
                           , envSegChan     = newC4
                           , envWTimerChan  = newC5
                           , envCBTimerChan = newC6 }
initState ∷ IO (TVar State)
initState = do
  let ref = AExcept (Just AnamnSuccess) ExAnamnesis ""
  let tile1 = GTile { tPos   = (0,0)
                    , tScale = (10,10)
                    , tInd   = (0,0)
                    , tSize  = (1,1)
                    , tT     = 0
                    , tMoves = False }
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  ls ← initLua
  ds ← initDrawState [tile1]
  let is = InputState { mouse1 = False
                      , mouse1Cache = (0.0,0.0)
                      , mouse2 = False
                      , mouse2Cache = (0.0,0.0)
                      , mouse3 = False
                      , mouse3Cache = (0.0,0.0) }
  atomically $ newTVar State { status       = ref
                             , logFunc      = lf
                             , windowSt     = Nothing
                             , drawSt       = ds
                             , luaSt        = ls
                             , inputState   = is
                             , sRecreate    = False }
