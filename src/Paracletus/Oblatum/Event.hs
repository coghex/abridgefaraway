{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Event where
-- key input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify',gets)
import Anamnesis
import Anamnesis.Data
import Anamnesis.Map
import Anamnesis.Util
import Anamnesis.World
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Epiklesis.Lua
import Epiklesis.Shell
import Paracletus.Data
import Paracletus.Oblatum.Data
import Paracletus.Oblatum.Input
import qualified Paracletus.Oblatum.GLFW as GLFW
-- user key strings from getKey function
evalKey ∷ GLFW.Window → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → GLFW.KeyLayout → Anamnesis ε σ ()
evalKey window k ks mk keyLayout = do
  st  ← get
  let oldLS  = luaSt st
      shCap  = shOpen $ luaShell oldLS
      selCap = inpCap $ inputState st
      cap    = shCap ∨ selCap
  -- glfw is parent thread, so this
  -- will close everything
  when (GLFW.keyCheck False keyLayout k "ESC") $ liftIO $ GLFW.setWindowShouldClose window True
  -- directional keys move camera in game
  -- windows, move cursor in menus
  when (GLFW.keyCheck cap keyLayout k "UP") $ do
    let oldIS = inputState st
    if (keyUp oldIS) then do
      if (ks ≡ GLFW.KeyState'Released) then do
        let newIS = oldIS { keyUp = False }
        modify' $ \s → s { inputState = newIS }
      else return ()
    else do
      if (ks ≡ GLFW.KeyState'Pressed) then do
        let newIS = oldIS { keyUp = True }
        modify' $ \s → s { inputState = newIS }
      else return ()
  when (GLFW.keyCheck cap keyLayout k "LFT") $ do
    let oldIS = inputState st
    if (keyLeft oldIS) then do
      if (ks ≡ GLFW.KeyState'Released) then do
        let newIS = oldIS { keyLeft = False }
        modify' $ \s → s { inputState = newIS }
      else return ()
    else do
      if (ks ≡ GLFW.KeyState'Pressed) then do
        let newIS = oldIS { keyLeft = True }
        modify' $ \s → s { inputState = newIS }
      else return ()
  when (GLFW.keyCheck cap keyLayout k "DWN") $ do
    let oldIS = inputState st
    if (keyDown oldIS) then do
      if (ks ≡ GLFW.KeyState'Released) then do
        let newIS = oldIS { keyDown = False }
        modify' $ \s → s { inputState = newIS }
      else return ()
    else do
      if (ks ≡ GLFW.KeyState'Pressed) then do
        let newIS = oldIS { keyDown = True }
        modify' $ \s → s { inputState = newIS }
      else return ()
  when (GLFW.keyCheck cap keyLayout k "RGT") $ do
    let oldIS = inputState st
    if (keyRight oldIS) then do
      if (ks ≡ GLFW.KeyState'Released) then do
        let newIS = oldIS { keyRight = False }
        modify' $ \s → s { inputState = newIS }
      else return ()
    else do
      if (ks ≡ GLFW.KeyState'Pressed) then do
        let newIS = oldIS { keyRight = True }
        modify' $ \s → s { inputState = newIS }
      else return ()
  -- shell displays over everything else on
  -- every window, executes in lua state
  when (GLFW.keyCheck False keyLayout k "SH") $ do
    if (ks ≡ GLFW.KeyState'Pressed) then do
      env ← ask
      let newLS  = oldLS { luaShell = openSh sh on }
          sh     = luaShell oldLS
          on     = not $ shOpen sh
          eventQ = envEventsChan env
      modify' $ \s → s { luaSt = newLS }
      liftIO $ atomically $ writeQueue eventQ $ EventLoaded
    else return ()
  -- captured keys allow for typing
  -- when in the shell or etc...
  when ((not (GLFW.keyCheck False keyLayout k "SH")) ∧ cap) $ do
    if (ks ≡ GLFW.KeyState'Pressed) then do
      env ← ask
      if (GLFW.keyCheck False keyLayout k "DEL")
      then do
        let newLS = oldLS { luaShell = removeShellString (luaShell oldLS) }
            eventQ = envEventsChan env
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      else if (GLFW.keyCheck False keyLayout k "SPC") then do
        let newSh = addShellString sh [' ']
            newLS = oldLS { luaShell = newSh }
            sh    = luaShell oldLS
            eventQ = envEventsChan env
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      -- evaluates lua command in state
      else if (GLFW.keyCheck False keyLayout k "RET") then do
        newLS ← liftIO $ evalShell env $ oldLS
        let eventQ = envEventsChan env
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      else if (GLFW.keyCheck False keyLayout k "TAB") then do
        let eventQ = envEventsChan env
            newSh  = tabShell sh $ luaCmds oldLS
            newLS  = oldLS { luaShell = newSh }
            sh     = luaShell oldLS
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      else if (GLFW.keyCheck False keyLayout k "LFA") then do
        let eventQ = envEventsChan env
            newSh  = leftShell sh
            sh     = luaShell oldLS
            newLS  = oldLS { luaShell = newSh }
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      else if (GLFW.keyCheck False keyLayout k "RGA") then do
        let eventQ = envEventsChan env
            newSh  = rightShell sh
            sh     = luaShell oldLS
            newLS  = oldLS { luaShell = newSh }
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      else if (GLFW.keyCheck False keyLayout k "UPA") then do
        let eventQ = envEventsChan env
            newSh  = upShell sh
            sh     = luaShell oldLS
            newLS  = oldLS { luaShell = newSh }
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      else if (GLFW.keyCheck False keyLayout k "DNA") then do
        let eventQ = envEventsChan env
            newSh  = downShell sh
            sh     = luaShell oldLS
            newLS  = oldLS { luaShell = newSh }
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      else if (GLFW.modifierKeysControl mk) then do
        if (GLFW.keyCheck False keyLayout k "C") then do
          let newSh  = resetShell sh
              newLS  = oldLS { luaShell = newSh }
              sh     = luaShell oldLS
              eventQ = envEventsChan env
          modify' $ \s → s { luaSt = newLS }
          liftIO $ atomically $ writeQueue eventQ $ EventLoaded
        else return ()
      else do
        ch ← liftIO $ GLFW.calcInpKey k mk
        let newSh  = addShellString sh ch
            newLS  = oldLS { luaShell = newSh }
            sh     = luaShell oldLS
            eventQ = envEventsChan env
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
    else if (ks ≡ GLFW.KeyState'Repeating) then do
      env ← ask
      if (GLFW.keyCheck False keyLayout k "LFA") then do
        let eventQ = envEventsChan env
            newSh  = leftShell sh
            sh     = luaShell oldLS
            newLS  = oldLS { luaShell = newSh }
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      else if (GLFW.keyCheck False keyLayout k "RTA") then do
        let eventQ = envEventsChan env
            newSh  = rightShell sh
            sh     = luaShell oldLS
            newLS  = oldLS { luaShell = newSh }
        modify' $ \s → s { luaSt = newLS }
        liftIO $ atomically $ writeQueue eventQ $ EventLoaded
      else return ()
    else return ()
  -- input for selected objects
  when (selCap) $ do
    if (ks ≡ GLFW.KeyState'Pressed) then do
      if (GLFW.keyCheck False keyLayout k "RTA") then do
        st ← get
        let ls      = luaSt st
            currWin = currentWindow ls
            newWin  = currWin { winElems = (inpSelectedElems IEDRight (winElems currWin)) }
            newLS   = ls { luaWindows = replaceWin newWin (luaWindows ls) }
        modify' $ \s → s { luaSt = newLS }
      else if (GLFW.keyCheck False keyLayout k "LFA") then do
        st ← get
        let ls      = luaSt st
            currWin = currentWindow ls
            newWin  = currWin { winElems = (inpSelectedElems IEDLeft (winElems currWin)) }
            newLS   = ls { luaWindows = replaceWin newWin (luaWindows ls) }
        modify' $ \s → s { luaSt = newLS }
      else if (GLFW.keyCheck False keyLayout k "DEL") then do
        st ← get
        let ls      = luaSt st
            currWin = currentWindow ls
            newWin  = currWin { winElems = (inpSelectedElems IEDDel (winElems currWin)) }
            newLS   = ls { luaWindows = replaceWin newWin (luaWindows ls) }
        modify' $ \s → s { luaSt = newLS }
      else if (GLFW.numCheck k > (-1)) then do
        st ← get
        let ls      = luaSt st
            currWin = currentWindow ls
            newWin  = currWin { winElems = (inpSelectedElems (IEDAdd (GLFW.numCheck k)) (winElems currWin)) }
            newLS   = ls { luaWindows = replaceWin newWin (luaWindows ls) }
        modify' $ \s → s { luaSt = newLS }
      else return ()
    else return ()

-- evaluates mouse input
evalMouse ∷ GLFW.Window → GLFW.MouseButton → GLFW.MouseButtonState → GLFW.ModifierKeys → Anamnesis ε σ ()
evalMouse win mb mbs _  = do
  env ← ask
  st  ← get
  let oldIS   = inputState st
      ls      = luaSt st
      thisWin = (luaWindows ls) !! (luaCurrWin ls)
  -- mouse button 1 press
  when ((mb ≡ GLFW.mousebutt1) ∧ ((winType thisWin) ≡ WinTypeMenu)) $ do
    if (mbs ≡ GLFW.MouseButtonState'Pressed) then do
      pos' ← liftIO $ GLFW.getCursorPos win
      let pos = convertPixels pos'
      -- save cache as raw pixels, not units
      let newIS = oldIS { mouse1      = True
                        , mouse1Cache = (realToFrac (fst pos'), realToFrac (snd pos')) }
      modify' $ \s → s { inputState = newIS }
      linkTest pos (winElems thisWin)
  -- mouse button 1 release
    else if (mbs ≡ GLFW.MouseButtonState'Released) then do
      let newIS = oldIS { mouse1 = False
                        , isElems = turnOffISElems (isElems oldIS) }
      modify' $ \s → s { inputState = newIS }
    else return ()
  -- mouse button 3 press
  when ((mb ≡ GLFW.mousebutt3) ∧ (winType thisWin ≡ WinTypeGame)) $ do
    if ((mbs ≡ GLFW.MouseButtonState'Pressed) ∧ ((mouse3 oldIS) ≡ False)) then do
      pos' ← liftIO $ GLFW.getCursorPos win
      let pos = ((realToFrac (fst pos')),(realToFrac (snd pos')))
          newIS = oldIS { mouse3 = True
                        , mouse3Cache = pos }
      modify' $ \s → s { inputState = newIS }
      --logDebug $ "mouse 3 click at: " ⧺ (show pos)
  -- mouse button 3 release
    else if ((mbs ≡ GLFW.MouseButtonState'Released) ∧ ((mouse3 oldIS) ≡ True)) then do
      if ((winType thisWin) ≡ WinTypeGame) then do
        case (findWorldData thisWin) of
          Just _ → do
            let newIS = oldIS { mouse3 = False }
            modify' $ \s → s { inputState = newIS }
            liftIO $ atomically $ writeQueue (envLCmdChan env) $ LoadCmdWorld ls
            --logDebug $ "mouse 3 unclick: " ⧺ (show (winCursor thisWin))
          Nothing → return ()
      else return ()
    else return ()
      
linkTest ∷ (Double,Double) → [WinElem] → Anamnesis ε σ ()
linkTest pos elems = do
  let links = filter isElemLink elems
  linkTestFunc False pos links

linkTestFunc ∷ Bool → (Double,Double) → [WinElem] → Anamnesis ε σ ()
-- if no link was clicked, reset input capture
linkTestFunc False  _     []           = do
  oldIS ← gets inputState
  oldLS ← gets luaSt
  env   ← ask
  let eventQ = envEventsChan env
    -- selecting -1 resets all boxes to false
  modify' $ \s → s { luaSt      = toggleMenuElem (-1) "NULL" oldLS
                   , inputState = oldIS
          { isElems = toggleSelectISF (-1) (isElems oldIS)
          , inpCap = False } }
  liftIO $ atomically $ writeQueue eventQ $ EventLoaded
--linkTestFunc True   _     []           = return ()
linkTestFunc True   _     _            = return ()
linkTestFunc linked (x,y) (link:links) = do
  let (buttWidth,buttHeight) = linkBox link
  case (posClose (buttWidth,buttHeight) (linkPos link) (x,y)) of
    True → do
      env ← ask
      let eventQ = envEventsChan env
      case (linkAct link) of
        LinkExit → do
          logDebug $ "quitting..."
          window' ← gets windowSt
          case window' of
            Just window → liftIO $ GLFW.setWindowShouldClose window True
            Nothing → logWarn "no window to exit"
        LinkLink win → do
          logDebug $ "following link to " ⧺ win
          liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdswitchWindow win)
        LinkBack → do
          ls ← gets luaSt
          let newWin  = (luaWindows ls) !! (luaLastWin ls)
          logDebug $ "going back to " ⧺ winTitle newWin
          liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdswitchWindow (winTitle newWin))
        LinkSelect n menu → do
          st ← get
          let ls = luaSt st
              is = inputState st
          modify' $ \s → s { inputState = toggleSelectIS n is
                           , luaSt      = toggleMenuElem n menu ls }
          liftIO $ atomically $ writeQueue eventQ $ EventLoaded
        LinkSlider n → do
          st ← get
          let ds = drawSt st
              ls = luaSt st
              is = inputState st
          modify' $ \s → s { inputState = toggleSliderIS n True is
                           , drawSt     = moveSlider x n ds
                           , luaSt      = moveLSSlider x n ls}
        LinkNULL → logError "linkNULL clicked"
      linkTestFunc True (x,y) links
    False → linkTestFunc linked (x,y) links

