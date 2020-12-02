{-# LANGUAGE Strict #-}
module Anamnesis.Event where
-- events and exceptions are handled
import Prelude()
import UPrelude
import Control.Concurrent (forkIO)
import Control.Monad.State.Class (modify,gets)
import Data.Time.Clock.System
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
    ls ← gets luaSt
    let keyLayout = lcKeyLayout $ luaConfig ls
    evalKey win k ks mk keyLayout
  (EventMouseButton win mb mbs mk) → evalMouse win mb mbs mk
  (EventLoadedLuaState ds) → modify $
    \s → s { drawSt = ds
           , sRecreate = True }
  -- callback for a loaded drawState
  (EventLoadedDrawState ds) → do
    modify $ \s → s { drawSt = ds }
    env ← ask
    let lCmdChan = envLCmdChan env
    liftIO $ atomically $ writeQueue lCmdChan $ LoadCmdVerts ds
  -- callback for a loaded luaState
  (EventLoadedWorld ls) → modify $
    \s → s { luaSt = ls }
  -- translates the lua draw state
  -- into the engine draw state
  (EventLoaded) → do
    env ← ask
    let lCmdChan = envLCmdChan env
    ls ← gets luaSt
    liftIO $ atomically $ writeQueue lCmdChan $ LoadCmdWin ls
  (EventLoadedVerts verts) → modify $
    \s → s { sVertCache = Just verts
           , sReload    = True }
  (EventLua command) → do
    case command of
      (LuaCmdnewWindow newWin) → do
        st ← get
        modify $ \s → s { luaSt = addWinToLuaState (luaSt st) newWin }
      (LuaCmdnewElem win e cache) → do
      -- if you wish to load more textures
      -- sRecreate must be set True
        st ← get
        case e of
          WinElemText _ _ _ → do
            let newLS = addElemToLuaState win e cache (luaSt st)
            modify $ \s → s { luaSt = newLS }
          WinElemLink _ _ _ → do
            let newLS = addElemToLuaState win e cache (luaSt st)
            modify $ \s → s { luaSt = newLS }
          WinElemBack _ → do
            let newLS = addElemToLuaState win e cache (luaSt st)
            modify $ \s → s { luaSt = newLS
                            , sRecreate = True }
          WinElemWorld _ _ _ → do
            let newLS = addElemToLuaState win e cache (luaSt st)
            modify $ \s → s { luaSt = newLS
                            , sRecreate = True }
          WinElemDyn _ _ → do
            let newLS = addElemToLuaState win e cache (luaSt st)
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
      (LuaCmdtoggleFPS) → do
        ls ← gets luaSt
        case (luaFPS ls) of
          Just _  → modify $ \s → s { luaSt = newLS }
            where newLS = ls { luaFPS = Nothing }
          Nothing → do
            starttime ← liftIO $ getSystemTime
            modify $ \s → s { sStartTime = starttime
                            , luaSt = newLS }
              where newLS = ls { luaFPS = Just 0 }
      (LuaError str) → logWarn str
      (LuaFind lfCommand) → case lfCommand of
        (LFScreenCursor) → do
          st ← get
          let ls = luaSt st
              currWin = (luaWindows ls) !! (luaCurrWin ls)
          logInfo $ "screenCursor: " ⧺ (show (winCursor currWin))
        (LFNULL)         → logError $ "lua NULL query"
      (LuaCmdNULL)   → logError $ "lua NULL command"
