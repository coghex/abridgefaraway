{-# LANGUAGE Strict #-}
module Anamnesis.Event where
-- events and exceptions are handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify, gets)
import Anamnesis
import Anamnesis.Data
import Anamnesis.Draw
import Anamnesis.Util
import Artos.Except
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Oblatum
import Paracletus.Oblatum.Event
import qualified Paracletus.Oblatum.GLFW as GLFW
-- reads event channel, then
-- executes events in order
processEvents ∷ Anamnesis ε σ ()
processEvents = do
  env ← ask
  event ← liftIO $ atomically $ tryReadQueue $ envEventsChan env
  case event of
    Just e → do
      processEvent e
      processEvents
    Nothing → return ()
-- case statement on each event
processEvent ∷ Event → Anamnesis ε σ ()
processEvent event = case event of
  -- this only throws GLFW errors
  (EventError err str) → do
    st ← get
    _  ← logExcept err ExParacletus str
    case (windowSt st) of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → logWarn $ "no glfw window to close"
  (EventKey window k _ ks mk) → do
    keyLayout ← importKeyLayout
    when (ks ≡ GLFW.KeyState'Pressed) $ evalKey window k ks mk keyLayout
  (EventLua command args) → do
    oldluaSt ← gets luaSt
    case command of
      LuaCmdnewWindow win → modify $ \s → s { luaSt = newluastate }
        where newluastate = LuaState { luaState = luaState oldluaSt
                                     , luaWindows = ((luaWindows oldluaSt) ⧺ [win]) }
      LuaCmdNULL → logError $ "lua NULL command"
      otherwise → logWarn $ "unknown lua command"
  (EventLoaded loadedType) → do
    -- loads a window from base lua file
    st ← get
    let tile1 = GTile { tPos   = (0,0)
                      , tScale = (10,10)
                      , tInd   = (0,0)
                      , tSize  = (1,1)
                      , tT     = 11 }
    --let newds = DrawState ((dsTiles (drawSt st)) ⧺ [newtile]) (dsTextB (drawSt st))
    let newds = DrawState [tile1] []
    modify $ \s → s { drawSt = newds
                    , sRecreate = True }
    logWarn $ "loaded event"
