{-# LANGUAGE Strict #-}
module Anamnesis.Event where
-- events and exceptions are handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify, gets)
import Anamnesis
import Anamnesis.Data
import Anamnesis.Util
import Anamnesis.World
import Artos.Data
import Artos.Except
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Epiklesis.Lua
import Epiklesis.World
import Paracletus.Data
import Paracletus.Draw
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
  -- this ony throws GLFW errors
  (EventError err str) → do
    st ← get
    _  ← logExcept err ExParacletus str
    case (windowSt st) of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → logWarn $ "no glfw window to close"
  (EventKey win k _ ks mk) → do
    keyLayout ← importKeyLayout
    evalKey win k ks mk keyLayout
  (EventMouseButton win mb mbs mk) → evalMouse win mb mbs mk
  (EventLoaded) → do
  -- translates the lua draw state
  -- into the engine draw state,
  -- called every window change
    logDebug $ "loaded event"
    env ← ask
    st  ← get
    let ls = luaSt st
        currWin = (luaWindows ls) !! (luaCurrWin ls)
        newDS   = loadDrawState ls
    modify $ \s → s { drawSt = newDS
                    , sRecreate = True }
  (EventLua command) → do
    case command of
      (LuaCmdnewWindow newWin) → do
        st ← get
        modify $ \s → s { luaSt = addWinToLuaState (luaSt st) newWin }
      (LuaCmdnewElem win e) → do
        case e of
          WinElemText _ _ _ → do
            st ← get
            let newLS = addElemToLuaState win e (luaSt st)
            modify $ \s → s { luaSt = newLS }
          WinElemBack _ → do
            st ← get
            let newLS = addElemToLuaState win e (luaSt st)
            modify $ \s → s { luaSt = newLS
                            , sRecreate = True }
          WinElemNULL → logError "null window element"
      (LuaCmdswitchWindow winName) → do
        env ← ask
        st  ← get
        let eventQ = envEventsChan env
            ls     = luaSt st
            wins   = luaWindows ls
            winNum = winToNum 0 wins winName
            winToNum ∷ Int → [Window] → String → Int
            winToNum _ []      _   = -1
            winToNum n (win:wins) name
              | (winTitle win) == name = n
              | otherwise = winToNum (n+1) wins name
        if (winNum < 0) then do
          logWarn $ "window " ⧺ winName ⧺ " not defined"
        else do
          modify $ \s → s { luaSt = changeCurrWin winNum ls }
          liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      (LuaError str) → logWarn str
      (LuaCmdNULL)   → logError $ "lua NULL command"
