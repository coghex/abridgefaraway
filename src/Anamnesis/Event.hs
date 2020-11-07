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
import Anamnesis.World
import Artos.Data
import Artos.Except
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Epiklesis.World
import Epiklesis.Shell
import Paracletus.Data
import Paracletus.Oblatum
import Paracletus.Oblatum.Data
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
  (EventLogDebug str) → logDebug str
  (EventKey window k _ ks mk) → do
    keyLayout ← importKeyLayout
    when (ks ≡ GLFW.KeyState'Pressed) $ evalKey window k ks mk keyLayout
  (EventMouseButton win mb mbs mk) → do
    evalMouse win mb mbs mk
  (EventCam pos) → do
    modify $ \s → s { cam3d = pos }
  (EventLua command args) → do
    oldluaSt ← gets luaSt
    oldds ← gets drawSt
    case command of
      LuaCmdnewWindow win → modify $ \s → s { luaSt = newluastate }
        where newluastate = LuaState { luaState = luaState oldluaSt
                                     , luaWindows = ((luaWindows oldluaSt) ⧺ [win]) }
      LuaCmdnewLuaWindow win → logDebug $ "newLuaWindow"-- ⧺ (lwName win)
      LuaCmdnewText win newtext → modify $ \s → s { luaSt = newluastate }
        where newluastate = LuaState (luaState oldluaSt) (addTextToLuaWindows win newtext (luaWindows oldluaSt))
      LuaCmdnewButton win newtext link → modify $ \s → s { luaSt = newluastate }
        where newluastate = LuaState (luaState oldluaSt) (addTextToLuaWindows win newtext (luaWindows oldluaSt))
      LuaCmdnewTile win tile → modify $ \s → s { luaSt = newluastate }
        where newluastate = LuaState (luaState oldluaSt) (addTileToLuaWindows win tile (luaWindows oldluaSt))
      LuaCmdnewLink win link → modify $ \s → s { luaSt = newluastate }
        where newluastate = LuaState (luaState oldluaSt) (addLinkToLuaWindows win link (luaWindows oldluaSt))
      LuaCmdnewMenu win link → modify $ \s → s { luaSt = newluastate }
        where newluastate = LuaState (luaState oldluaSt) (addMenuToLuaWindows win link (luaWindows oldluaSt))
      LuaCmdnewMenuElement menu element → modify $ \s → s { luaSt = newluastate }
        where newluastate = LuaState (luaState oldluaSt) (addElementToMenu menu element (luaWindows oldluaSt))
      LuaCmdnewWorld menu world → modify $ \s → s { luaSt = newluastate }
        where newluastate = LuaState (luaState oldluaSt) (addWorldToWindows menu world (luaWindows oldluaSt))
      LuaCmdswitchWindow winName → do
        env ← ask
        let eventQ = envEventsChan env
        luaState ← gets luaSt
        sc ← gets screenCursor
        let windows = luaWindows luaState
            winNum  = winToNum 0 windows winName
            winToNum ∷ Int → [Window] → String → Int
            winToNum _ []         name = -1
            winToNum n (win:wins) name
              | (winTitle win) == name = n
              | otherwise = winToNum (n+1) wins name
        modify $ \s → s { currentWin = winNum }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded 1
      LuaError str → logWarn str
      LuaCmdNULL → logError $ "lua NULL command"
      --otherwise → logWarn $ "unknown lua command"
  (EventUpdateSegs seg) → do
    env ← ask
    st ← get
    let currWin    = (luaWindows (luaSt st)) !! (currentWin st)
    if ((winType currWin) == WinTypeGame)
    then do
      let oldsegs    = zoneSegs $ oldZone
          oldZone    = head $ worldZone $ windowWorld $ currWin
          newsegs    = findAndReplaceSegment seg oldsegs
          newls      = LuaState (luaState (luaSt st)) (newWins)
          newWins    = replaceWindow newWin (luaWindows (luaSt st))
          newWin     = currWin { windowWorld = newWorld }
          newWorld   = (windowWorld currWin) { worldZone = [newZone] }
          newZone    = Zone (zoneIndex oldZone) (newsegs)
          newds = reloadDrawSt env st
      liftIO $ reloadScreenCursor env (screenCursor st)
      modify $ \s → s { drawSt = newds
                      , luaSt = newls }
    else return ()
  (EventLoaded loadedType) → do
    -- translates lua draw state to engine state
    env ← ask
    st ← get
    -- indexes the current window
    let menuwindow = (luaWindows (luaSt st)) !! (currentWin st)
    -- loads a background for a menu
    let tile1 = GTile { tPos   = (0,0)
                      , tScale = (32,24)
                      , tInd   = (0,0)
                      , tSize  = (1,1)
                      , tT     = 19
                      , tMoves = False }
    -- loads the tiles in lua state
    let modtiles = calcTiles menuwindow
    -- loads tiles from world object
    let worldtiles = calcWorldTiles (screenCursor st) menuwindow (length modtiles)
    let newds = DrawState ([tile1]⧺modtiles⧺worldtiles) (calcTextBoxs menuwindow) MBNULL ShellNULL
        newsc = initScreenCursor (cam3d st) (gamecam3d st) (screenCursor st)
    modify $ \s → s { drawSt = newds
                    , screenCursor  = newsc
                    , sRecreate = True }
    logDebug $ "loaded event"
    if ((winType menuwindow) == WinTypeGame)
    then liftIO $ reloadScreenCursor env (screenCursor st)
    else do
      liftIO $ atomically $ writeChan (envWTimerChan env) TStart
      liftIO $ atomically $ writeChan (envSCChan env) (screenCursor st)

-- these functions are seperate so that
-- they can be easily recursive
addTileToLuaWindows ∷ String → WinTile → [Window] → [Window]
addTileToLuaWindows wn wt ws = map (addTileToLuaWindow wn wt) ws

addTileToLuaWindow ∷ String → WinTile → Window → Window
addTileToLuaWindow wn wt (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw)
  | (wn == name) = (Window name oldt oldb oldwt oldlinks (oldtiles⧺[wt]) oldm oldw)
  | otherwise    = (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw)

addTextToLuaWindows ∷ String → WinText → [Window] → [Window]
addTextToLuaWindows wn wt ws = map (addTextToLuaWindow wn wt) ws

addTextToLuaWindow ∷ String → WinText → Window → Window
addTextToLuaWindow wn wt (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw)
  | (wn == name) = (Window name oldt oldb (oldwt⧺[wt]) oldlinks oldtiles oldm oldw)
  | otherwise    = (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw)

addLinkToLuaWindows ∷ String → WinLink → [Window] → [Window]
addLinkToLuaWindows wn wl ws = map (addLinkToLuaWindow wn wl) ws

addLinkToLuaWindow ∷ String → WinLink → Window → Window
addLinkToLuaWindow wn wl (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw)
  | (wn == name) = (Window name oldt oldb oldwt (oldlinks⧺[wl]) oldtiles oldm oldw)
  | otherwise    = (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw)

addMenuToLuaWindows ∷ String → WinMenu → [Window] → [Window]
addMenuToLuaWindows wn wm ws = map (addMenuToLuaWindow wn wm) ws

addMenuToLuaWindow ∷ String → WinMenu → Window → Window
addMenuToLuaWindow wn wm (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw)
  | (wn == name) = (Window name oldt oldb oldwt oldlinks oldtiles (oldm⧺[wm]) oldw)
  | otherwise    = (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw)

addElementToMenu ∷ String → WinElem → [Window] → [Window]
addElementToMenu menu element ws = map (addElementToMenuWindow menu element) ws

addElementToMenuWindow ∷ String → WinElem → Window → Window
addElementToMenuWindow menu element (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw) = Window name oldt oldb oldwt oldlinks oldtiles (map (addElemToWindowsMenu menu element) oldm) oldw

addElemToWindowsMenu ∷ String → WinElem → WinMenu → WinMenu
addElemToWindowsMenu menu element (WinMenu name pos elems)
   | menu == name = WinMenu name pos (elems⧺[element])
   | otherwise = WinMenu name pos elems

addWorldToWindows ∷ String → World → [Window] → [Window]
addWorldToWindows menu world ws = map (addWorldToWindow menu world) ws

addWorldToWindow ∷ String → World → Window → Window
addWorldToWindow menu world (Window name oldt oldb oldwt oldlinks oldtiles oldm oldw)
  | menu == name = Window name oldt oldb oldwt oldlinks oldtiles oldm world
  | otherwise    = Window name oldt oldb oldwt oldlinks oldtiles oldm WorldNULL
