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
import Paracletus.Oblatum.Data
import qualified Paracletus.Oblatum.GLFW as GLFW
-- user key strings from getKey function
evalKey ∷ GLFW.Window → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → GLFW.KeyLayout → Anamnesis ε σ ()
evalKey window k ks mk keyLayout = do
  st  ← get
  let oldLS = luaSt st
      shCap = shOpen $ luaShell oldLS
      cap   = shCap
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
  when ((not (GLFW.keyCheck False keyLayout k "SH")) ∧ cap ∧ (ks ≡ GLFW.KeyState'Pressed)) $ do
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
      linkTest pos (winElems thisWin)
      -- save cache as raw pixels, not units
      let newIS = oldIS { mouse1      = True
                        , mouse1Cache = (realToFrac (fst pos'), realToFrac (snd pos')) }
      modify' $ \s → s { inputState = newIS }
  -- mouse button 1 release
    else if (mbs ≡ GLFW.MouseButtonState'Released) then do
      let newIS = oldIS { mouse1 = False }
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
      
-- TODO: un-hardcode the pixels here
convertPixels ∷ ∀ a. (Fractional a, Num a) ⇒ (a,a) → (a,a)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1280.0 / 2.0)) / 64.0)
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

posClose ∷ (Double,Double) → (Double,Double) → (Double,Double) → Bool
posClose (buttWidth,buttHeight) (x1,y1) (x2,y2)
  | ((abs(x1 - x2)) < buttWidth) && ((abs(y1 - y2)) < buttHeight) = True
  | otherwise = False

linkTest ∷ (Double,Double) → [WinElem] → Anamnesis ε σ ()
linkTest pos elems = do
  let links = filter isElemLink elems
  linkTestFunc pos links

linkTestFunc ∷ (Double,Double) → [WinElem] → Anamnesis ε σ ()
linkTestFunc _     []           = return ()
linkTestFunc (x,y) (link:links) = do
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
        LinkNULL → logError "linkNULL clicked"
      linkTest (x,y) links
    False → linkTest (x,y) links

isElemLink ∷ WinElem → Bool
isElemLink (WinElemLink _ _ _) = True
isElemLink (WinElemWorld _ _ _) = False
isElemLink (WinElemText _ _ _) = False
isElemLink (WinElemBack _ )    = False
isElemLink (WinElemDyn _ _)    = False
isElemLink (WinElemMenu _ _ _) = False
isElemLink (WinElemNULL)       = False

moveCamWithKeys ∷ Anamnesis ε σ ()
moveCamWithKeys = do
  env ← ask
  st ←  get
  let ls      = luaSt st
      currWin = currentWindow ls
  if ((winType currWin) ≡ WinTypeGame) then do
    case (findWorldData currWin) of
      Just (_,wd) → do
        let oldIS    = inputState st
            dir      = case (findDir oldIS) of
                         Just d  → d
                         Nothing → CardNULL
            oldcam   = winCursor currWin
            newIS    = oldIS { keyAccel = newaccel }
            newaccel = decell $ accelIS dir (keyAccel oldIS)
            newcam   = keyMoveCam newaccel oldcam
            newSC    = moveScreenCursor newcam
            newWD    = wd { wdCam = newSC }
            newWin'  = replaceWorldData currWin newWD
            newWin   = newWin' { winCursor = newcam }
            newWins  = findAndReplaceWindow newWin (luaWindows ls)
            newLS   = ls { luaWindows = newWins }
        if ((newaccel) ≡ (0.0,0.0)) then liftIO (atomically (writeQueue (envLCmdChan env) (LoadCmdWorld newLS))) else return ()
        modify' $ \s → s { luaSt = newLS
                         , inputState = newIS }
      Nothing     → return ()
  else return ()

-- many keys can be held at once,
-- we define bahavior of all
-- combinations here
findDir ∷ InputState → Maybe Cardinal
findDir is = if      (keyUp    is) ∧ (keyLeft  is) ∧ (keyRight is) ∧ (keyDown  is) then Nothing
             else if (keyUp    is) ∧ (keyLeft  is) ∧ (keyRight is) then Just North
             else if (keyUp    is) ∧ (keyLeft  is) ∧ (keyDown  is) then Just West
             else if (keyUp    is) ∧ (keyRight is) ∧ (keyDown  is) then Just East
             else if (keyUp    is) ∧ (keyLeft  is) then Just NorthWest
             else if (keyUp    is) ∧ (keyRight is) then Just NorthEast
             else if (keyUp    is) ∧ (keyDown  is) then Nothing
             else if (keyUp    is) then Just North
             else if (keyDown  is) ∧ (keyLeft  is) ∧ (keyRight is) then Just South
             else if (keyDown  is) ∧ (keyLeft  is) then Just SouthWest
             else if (keyDown  is) ∧ (keyRight is) then Just SouthEast
             else if (keyDown  is) then Just South
             else if (keyLeft  is) ∧ (keyRight is) then Nothing
             else if (keyLeft  is) then Just West
             else if (keyRight is) then Just East
             else Nothing

-- accelerate the inputstate
accelIS ∷ Cardinal → (Float,Float) → (Float,Float)
accelIS North (x,y) = (x, 1.1*(y - 0.1))
accelIS West  (x,y) = (1.1*(x + 0.1), y)
accelIS South (x,y) = (x, 1.1*(y + 0.1))
accelIS East  (x,y) = (1.1*(x - 0.1), y)
accelIS NorthWest (x,y) = (1.1*(x + 0.1), 1.1*(y - 0.1))
accelIS NorthEast (x,y) = (1.1*(x - 0.1), 1.1*(y - 0.1))
accelIS SouthWest (x,y) = (1.1*(x + 0.1), 1.1*(y + 0.1))
accelIS SouthEast (x,y) = (1.1*(x - 0.1), 1.1*(y + 0.1))
accelIS CardNULL (x,y) = (x,y)

decell ∷ (Float,Float) → (Float,Float)
decell (x,y)
  | ((abs x) < 0.01) ∧ ((abs y) < 0.01) = (0.0,0.0)
  | ((abs x) < 0.01) = (0.0,(y / 1.1))
  | ((abs y) < 0.01) = ((x / 1.1),0.0)
  | otherwise = ((x / 1.1),(y / 1.1))

keyMoveCam ∷ (Float,Float) → (Float,Float,Float) → (Float,Float,Float)
keyMoveCam (i,j) (x,y,z) = (x+i,y+j,z)

moveCamWithMouse ∷ Anamnesis ε σ ()
moveCamWithMouse = do
  st ← get
  let win' = windowSt st
  case win' of
    Just win → do
      let currWin = currentWindow ls
          ls = luaSt st
      if ((winType currWin) ≡ WinTypeGame) then do
        case (findWorldData currWin) of
          Just (_,wd) → do
            pos' ← liftIO $ GLFW.getCursorPos win
            let pos = ((realToFrac (fst pos')),(realToFrac (snd pos')))
                oldpos = mouse3Cache (inputState st)
                diff = (((fst pos)-(fst oldpos)),((snd pos)-(snd oldpos)))
                oldcam = winCursor currWin
                newcam = moveCam oldcam diff
                moveCam ∷ (Float,Float,Float) → (Float,Float) → (Float,Float,Float)
                moveCam (x1,y1,z1) (x2,y2) = (x1+x2',y1-y2',z1) where (x2',y2') = (x2/3.6,y2/3.6)
                oldIS = inputState st
                newIS = oldIS { mouse3 = True
                              , mouse3Cache = ((fst pos),(snd pos)) }
                newSC = moveScreenCursor newcam
                newWD = wd { wdCam = newSC }
                newWin' = replaceWorldData currWin newWD
                newWin  = newWin' { winCursor = newcam }
                newWins = findAndReplaceWindow newWin (luaWindows ls)
                newLS = ls { luaWindows = newWins }
            modify' $ \s → s { luaSt = newLS
                             , inputState = newIS }
          Nothing → return ()
      else return ()
    Nothing → return ()

moveScreenCursor ∷ (Float,Float,Float) → (Float,Float)
moveScreenCursor (x,y,_) = (-0.05*x,-0.05*y)
