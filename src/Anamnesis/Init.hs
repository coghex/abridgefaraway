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
import Data.Time.Clock.System
import Anamnesis
import Anamnesis.Data
import Anamnesis.Draw
import Paracletus.Data
import Paracletus.Oblatum
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
  newQ1 ← newQueue
  newQ2 ← newCmdQueue
  newC1 ← newTChan
  atomically $ newTVar Env { envEventsChan  = newQ1
                           , envLCmdChan    = newQ2
                           , envLTimerChan  = newC1 }
initState ∷ IO (TVar State)
initState = do
  let ref = AExcept (Just AnamnSuccess) ExAnamnesis ""
  let tile1 = GTileUncached { tPos   = (0,0)
                            , tScale = (10,10)
                            , tInd   = (0,0)
                            , tSize  = (1,1)
                            , tT     = 0
                            , tTile  = False
                            , tMoves = False }
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  ls ← initLua
  ds ← initDrawState [tile1]
  starttime ← getSystemTime
  let is = initInputState
  atomically $ newTVar State { status       = ref
                             , logFunc      = lf
                             , windowSt     = Nothing
                             , drawSt       = ds
                             , luaSt        = ls
                             , inputState   = is
                             , sVertCache   = Nothing
                             , sTest        = 0
                             , sRecreate    = False
                             , sReload      = False
                             , sStartTime   = starttime
                             , sFPS         = (60.0,0)
                             , sTick        = Nothing }
