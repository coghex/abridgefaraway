{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Event where
-- key input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify',gets)
import Anamnesis
import Anamnesis.Data
import Anamnesis.Draw
import Anamnesis.Util
import Anamnesis.Map
import Anamnesis.World
import Epiklesis.Shell
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Paracletus.Oblatum.Data
import qualified Paracletus.Oblatum.GLFW as GLFW
-- user key strings from getKey function
evalKey ∷ GLFW.Window → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → GLFW.KeyLayout → Anamnesis ε σ ()
evalKey window k ks mk keyLayout = do
  st ← get
  -- capture will disable most keys for typing
  let cap = sShell st
  when (GLFW.keyCheck False keyLayout k "ESC") $ liftIO $ GLFW.setWindowShouldClose window True
  when (GLFW.keyCheck cap keyLayout k "UPP") $ do
    let newcursor = (moveCursor 1 (cursor st) North)
    modify' $ \s → s { cursor = newcursor }
    logDebug $ "cursor: " ⧺ (show newcursor) ⧺ ", cam3d: " ⧺ (show (cam3d st))
    return ()
  when (GLFW.keyCheck cap keyLayout k "DWN") $ do
    let newcursor = (moveCursor 1 (cursor st) South)
    modify' $ \s → s { cursor = newcursor }
    logDebug $ "cursor: " ⧺ (show newcursor) ⧺ ", cam3d: " ⧺ (show (cam3d st))
    return ()
  when (GLFW.keyCheck cap keyLayout k "RGT") $ do
    let newcursor = (moveCursor 1 (cursor st) East)
    modify' $ \s → s { cursor = newcursor }
    logDebug $ "cursor: " ⧺ (show newcursor) ⧺ ", cam3d: " ⧺ (show (cam3d st))
    return ()
  when (GLFW.keyCheck cap keyLayout k "LFT") $ do
    let newcursor = (moveCursor 1 (cursor st) West)
    modify' $ \s → s { cursor = newcursor }
    logDebug $ "cursor: " ⧺ (show newcursor) ⧺ ", cam3d: " ⧺ (show (cam3d st))
    return ()
  when ((GLFW.keyCheck False keyLayout k "SH") && (ks == GLFW.KeyState'Pressed)) $ do
    if (sShell st) then do
      let newds = (drawSt st) { dsShell = ShellNULL }
      modify' $ \s → s { drawSt = newds
                       , sShell = False }
      return ()
    else do
      let newds = initShell (drawSt st)
      modify' $ \s → s { drawSt = newds
                       , sShell = True }
      return ()
  when (GLFW.keyCheck cap keyLayout k "K") $ do
    let newcam3d  = (moveCursor 1.0 (cam3d st) North)
    modify' $ \s → s { cam3d  = newcam3d }
    logDebug $ "cursor: " ⧺ (show (cursor st)) ⧺ ", cam3d: " ⧺ (show newcam3d)
    return ()
  when (GLFW.keyCheck cap keyLayout k "C") $ do
    let newDS = (addTile (drawSt st))
    modify' $ \s → s { drawSt = newDS }
    logDebug $ "adding test tile..."
    return ()
  when (GLFW.keyCheck cap keyLayout k "J") $ do
    let newcam3d  = (moveCursor 1.0 (cam3d st) South)
    modify' $ \s → s { cam3d  = newcam3d }
    logDebug $ "cursor: " ⧺ (show (cursor st)) ⧺ ", cam3d: " ⧺ (show newcam3d)
    return ()
  when (GLFW.keyCheck cap keyLayout k "L") $ do
    let newcam3d  = (moveCursor 1.0 (cam3d st) East)
    modify' $ \s → s { cam3d  = newcam3d }
    logDebug $ "cursor: " ⧺ (show (cursor st)) ⧺ ", cam3d: " ⧺ (show newcam3d)
    return ()
  when (GLFW.keyCheck cap keyLayout k "H") $ do
    let newcam3d  = (moveCursor 1.0 (cam3d st) West)
    modify' $ \s → s { cam3d  = newcam3d }
    logDebug $ "cursor: " ⧺ (show (cursor st)) ⧺ ", cam3d: " ⧺ (show newcam3d)
    return ()
  when ((not (GLFW.keyCheck False keyLayout k "SH")) ∧ (ks == GLFW.KeyState'Pressed) ∧ (sShell st)) $ do
    if (GLFW.keyCheck False keyLayout k "DEL")
    then do
      modify' $ \s → s { drawSt = removeShellString (drawSt st) }
    else if (GLFW.keyCheck False keyLayout k "RET")
    then do
      let ls = luaState $ luaSt st
      newds ← liftIO $ evalShell (drawSt st) ls
      modify' $ \s → s { drawSt = newds }
    else if (GLFW.keyCheck False keyLayout k "SPC")
    then do
      let newds = addShellString (drawSt st) [' ']
      modify' $ \s → s { drawSt = newds }
    else do
      ch ← liftIO $ GLFW.calcInpKey k mk
      modify' $ \s → s { drawSt = addShellString (drawSt st) ch }
    return ()

-- evaluates mouse input
evalMouse ∷ GLFW.Window → GLFW.MouseButton → GLFW.MouseButtonState → GLFW.ModifierKeys → Anamnesis ε σ ()
evalMouse win mb mbs _  = do
  env ← ask
  st ← get
  let windows = luaWindows (luaSt st)
      thisWindow = windows !! (currentWin st)
      isold = inputState st
      oldds = drawSt st
  when (((mouse3 isold) == False) && (mb == GLFW.mousebutt3) && (mbs == GLFW.MouseButtonState'Pressed) && ((winType thisWindow) == WinTypeGame)) $ do
    pos' ← liftIO $ GLFW.getCursorPos win
    let pos = ((realToFrac (fst pos')),(realToFrac (snd pos')))
    let newis = isold { mouse3 = True
                      , mouse3Cache = pos }
    modify' $ \s → s { inputState = newis }
  when (((mouse3 isold) == True) && (mb == GLFW.mousebutt3) && (mbs == GLFW.MouseButtonState'Released) && ((winType thisWindow) == WinTypeGame)) $ do
    let newis = isold { mouse3 = False }
    let ((cx,cy),_) = screenCursor st
    modify' $ \s → s { inputState = newis }
    liftIO $ reloadScreenCursor env (screenCursor st)
    (x,y) ← liftIO $ GLFW.getCursorPos win
    let (x',y') = convertPixels (x,y)
    logDebug $ "mouse 3 unclick at x: " ⧺ (show x') ⧺ ", y: " ⧺ (show y') ⧺ " game cam is: (" ⧺ (show cx) ⧺ ", " ⧺ (show cy) ⧺ ")"
  when (((mouse2 isold) == False) && (mb == GLFW.mousebutt2) && (mbs == GLFW.MouseButtonState'Pressed) && ((winType thisWindow) == WinTypeGame)) $ do
    let newis = isold { mouse2 = True }
    modify' $ \s → s { inputState = newis }
  when (((mouse2 isold) == True) && (mb == GLFW.mousebutt2) && (mbs == GLFW.MouseButtonState'Released) && ((winType thisWindow) == WinTypeGame)) $ do
    (x,y) ← liftIO $ GLFW.getCursorPos win
    let newis = isold { mouse2 = False }
        (x',y') = convertPixels (x,y)
    modify' $ \s → s { inputState   = newis }
    logDebug $ "mouse click 2 at x: " ⧺ (show x') ⧺ ", y: " ⧺ (show y') ⧺ " | screenCursor: " ⧺ (show (screenCursor st))
  when ((mb == GLFW.mousebutt1) && ((winType thisWindow) == (WinTypeMenu))) $ do
    (x,y) ← liftIO $ GLFW.getCursorPos win
    let (x',y') = convertPixels (x,y)
    linkTest (x',y') (windowLinks thisWindow)
    --logDebug $ "mouse click 1 at x: " ⧺ (show x') ⧺ ", y: " ⧺ (show y')
  when (((mouse1 isold) == False) && (mb == GLFW.mousebutt1) && (mbs == GLFW.MouseButtonState'Pressed) && ((winType thisWindow) == (WinTypeGame))) $ do
    (x,y) ← liftIO $ GLFW.getCursorPos win
    let (x',y') = convertPixels (x,y)
    let newis = isold { mouse1 = True
                      , mouse1Cache = (realToFrac(x'),realToFrac(y'))}
    modify' $ \s → s { inputState = newis }
  when ((mb == GLFW.mousebutt1) && (mbs == GLFW.MouseButtonState'Released) && ((winType thisWindow) == (WinTypeGame))) $ do
    let newis = isold { mouse1 = False }
    let newds = DrawState (dsTiles oldds) (dsTextB oldds) MBNULL ShellNULL
    --logDebug $ "mouse unclick 1, mousebox: " ⧺ (show (dsMBox oldds))
    modify' $ \s → s { inputState = newis
                     , drawSt     = newds }

-- test the mouse click against every link
linkTest ∷ (Double,Double) → [WinLink] → Anamnesis ε σ ()
linkTest _     []           = return ()
linkTest (x,y) (link:links) = do
  --logDebug $ "linkTest: " ⧺ (linkAction link)
  case (posClose (buttWidth,buttHeight) (linkPos link) (x,y)) of
    True  → do
      env ← ask
      let eventQ = envEventsChan env
      case (linkAction link) of
        "link" → do
          logDebug $ "following link to " ⧺ (linkLink link)
          liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdswitchWindow (linkLink link)) ""
        "sliderLeft" → do
          logDebug $ "sliderLeft"
        "action" → do
          logDebug $ "quitting..."
          window' ← gets windowSt
          case window' of
            Just window → liftIO $ GLFW.setWindowShouldClose window True
            Nothing → logWarn "no window to exit"
        action → logWarn $ "no known action " ⧺ action
      linkTest (x,y) links
    False → linkTest (x,y) links
  where (buttWidth,buttHeight) = linkSize link

convertPixels ∷ ∀ a. (Fractional a, Num a) ⇒ (a,a) → (a,a)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1280.0 / 2.0)) / 64.0)
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

posClose ∷ (Double,Double) → (Double,Double) → (Double,Double) → Bool
posClose (buttWidth,buttHeight) (x1,y1) (x2,y2)
  | ((abs(x1 - x2)) < buttWidth) && ((abs(y1 - y2)) < buttHeight) = True
  | otherwise = False

-- these functions preform actions with the mouse
drawBoxWithMouse ∷ Anamnesis ε σ ()
drawBoxWithMouse = do
  st ← get
  let win' = windowSt st
  case win' of
    Just win → do
      posraw ← liftIO $ GLFW.getCursorPos win
      let pos' = convertPixels posraw
      let pos = ((realToFrac (fst pos')),(realToFrac (snd pos')))
          oldpos = (mouse1Cache (inputState st))
          oldds = drawSt st
          newds = DrawState (dsTiles oldds) (dsTextB oldds) (MouseBox oldpos pos) (dsShell oldds)
      modify' $ \s → s { drawSt = newds }
    Nothing → return ()

moveCamWithMouse ∷ Anamnesis ε σ ()
moveCamWithMouse = do
  st ← get
  let win' = windowSt st
  case win' of
    Just win → do
      pos' ← liftIO $ GLFW.getCursorPos win
      let pos = ((realToFrac (fst pos')),(realToFrac (snd pos')))
          oldpos = mouse3Cache (inputState st)
          diff = (((fst pos)-(fst oldpos)),((snd pos)-(snd oldpos)))
          oldcam = gamecam3d st
          newcam = moveCam oldcam diff
          moveCam ∷ (Float,Float,Float) → (Float,Float) → (Float,Float,Float)
          moveCam (x1,y1,z1) (x2,y2) = (x1+x2',y1-y2',z1) where (x2',y2') = (x2/3.6,y2/3.6)
          isold = inputState st
          newis = isold { mouse3 = True
                        , mouse3Cache = ((fst pos),(snd pos)) }
          luawin = (luaWindows (luaSt st)) !! (currentWin st)
          modtiles = calcTiles luawin
          newsc = moveScreenCursor (cam3d st) newcam $ screenCursor st
          worldtiles = calcWorldTiles newsc luawin $ length modtiles
          tile1 = head $ dsTiles (drawSt st)
          newds = DrawState ([tile1]⧺modtiles⧺worldtiles) (dsTextB (drawSt st)) (dsMBox (drawSt st)) (dsShell (drawSt st))
      modify' $ \s → s { drawSt = newds
                       , gamecam3d = newcam
                       , screenCursor = newsc
                       , inputState = newis }
    Nothing → return ()

-- initializes the camera
initScreenCursor ∷ (Float,Float,Float) → (Float,Float,Float) → ((Float,Float),(Int,Int)) → ((Float,Float),(Int,Int))
initScreenCursor oldcam3d oldgcam3d oldsc = moveScreenCursor oldcam3d oldgcam3d oldsc

moveScreenCursor ∷ (Float,Float,Float) → (Float,Float,Float) → ((Float,Float),(Int,Int)) → ((Float,Float),(Int,Int))
moveScreenCursor (x1,y1,_) (x2,y2,_) (( _, _),(cw,ch)) = ((cx,cy),(cw,ch))
  where cx = -0.05*(x2-x1)
        cy = -0.05*(y2-y1)

reloadDrawSt ∷ Env → State → DrawState
reloadDrawSt _   st = newds
    where luawin = (luaWindows (luaSt st)) !! (currentWin st)
          modtiles = calcTiles luawin
          newsc = moveScreenCursor (cam3d st) (gamecam3d st) $ screenCursor st
          worldtiles = calcWorldTiles newsc luawin $ length modtiles

          tile1 = head $ dsTiles (drawSt st)
          newds = DrawState ([tile1]⧺modtiles⧺worldtiles) (dsTextB (drawSt st)) (dsMBox (drawSt st)) (dsShell (drawSt st))
