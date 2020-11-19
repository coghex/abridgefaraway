{-# LANGUAGE Strict #-}
module Anamnesis.Event where
-- events and exceptions are handled
import Prelude()
import UPrelude
import Control.Concurrent (forkIO)
import Control.Monad.State.Class (modify)
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
import Epiklesis.Module
import Epiklesis.World
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
  (EventLogDebug str) → logDebug str
  (EventKey win k _ ks mk) → do
    keyLayout ← importKeyLayout
    evalKey win k ks mk keyLayout
  (EventMouseButton win mb mbs mk) → evalMouse win mb mbs mk
  (EventUpdateSegs (SegUpdateData SegOpAdd zoneInd segInd segDat)) → do
    env ← ask
    st  ← get
    let ls = luaSt st
        currWin = (luaWindows ls) !! (luaCurrWin ls)
    if ((winType currWin) ≡ WinTypeGame) then do
      case (findWorldData currWin) of
        Just (wp,wd) → do
          let newWD = findAndReplaceSegment (wpZSize wp) zoneInd segInd segDat wd

              newWin = replaceWorldData currWin newWD
              newWins = findAndReplaceWindow newWin (luaWindows ls)
              newLS = ls { luaWindows = newWins }
          liftIO $ reloadScreenCursor env ((wdCam newWD),(wdCSize newWD))
          modify $ \s → s { luaSt  = newLS }
        Nothing → logWarn "no world found"
    else return ()
  (EventUpdateSegs (SegUpdateData SegOpDel _       _      _     )) → logDebug "segOpDel"
  (EventLoaded) → do
  -- translates the lua draw state
  -- into the engine draw state,
  -- called every window change
    --logDebug $ "loaded event"
    env ← ask
    st  ← get
    let ls = luaSt st
        newDS   = loadDrawState ls
        currWin = (luaWindows ls) !! (luaCurrWin ls)
    if ((winType currWin) ≡ WinTypeGame) then do
      case (findWorldData currWin) of
        Just (_,wd) → do 
          liftIO $ atomically $ writeChan (envWTimerChan env) TStart
          liftIO $ atomically $ writeChan (envCamChan env) ((wdCam wd),(wdCSize wd))
        Nothing → return ()
    else return ()
    modify $ \s → s { drawSt = newDS
                    , sRecreate = True }
  (EventLua command) → do
    case command of
      (LuaCmdnewWindow newWin) → do
        st ← get
        modify $ \s → s { luaSt = addWinToLuaState (luaSt st) newWin }
      (LuaCmdnewElem win e) → do
      -- if you wish to load more textures
      -- sRecreate must be set True
        st ← get
        case e of
          WinElemText _ _ _ → do
            let newLS = addElemToLuaState win e (luaSt st)
            modify $ \s → s { luaSt = newLS }
          WinElemLink _ _ _ → do
            let newLS = addElemToLuaState win e (luaSt st)
            modify $ \s → s { luaSt = newLS }
          WinElemBack _ → do
            let newLS = addElemToLuaState win e (luaSt st)
            modify $ \s → s { luaSt = newLS
                            , sRecreate = True }
          WinElemWorld _ _ _ → do
            let newLS = addElemToLuaState win e (luaSt st)
            modify $ \s → s { luaSt = newLS
                            , sRecreate = True }
          WinElemNULL → logError "null window element"
      (LuaCmdloadModule str) → do
        env ← ask
        st  ← get
        modNew ← liftIO $ loadModule env st ModuleUser str
        let oldLS = luaSt st
            newLS = oldLS { luaModules = (luaModules oldLS) ⧺ [modNew] }
        modify $ \s → s { luaSt = newLS }
      (LuaCmdswitchWindow winName) → do
        env ← ask
        st  ← get
        let eventQ = envEventsChan env
            ls     = luaSt st
            lwins  = luaWindows ls
            winNum = winToNum 0 lwins winName
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
      (LuaFind lfCommand) → case lfCommand of
        (LFScreenCursor) → do
          st ← get
          let ls = luaSt st
              currWin = (luaWindows ls) !! (luaCurrWin ls)
          logInfo $ "screenCursor: " ⧺ (show (winCursor currWin))
        (LFNULL)         → logError $ "lua NULL query"
      (LuaCmdNULL)   → logError $ "lua NULL command"
