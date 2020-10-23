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
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import qualified Paracletus.Oblatum.GLFW as GLFW
-- user key strings from getKey function
evalKey ∷ GLFW.Window → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → GLFW.KeyLayout → Anamnesis ε σ ()
evalKey window k _  _  keyLayout = do
  st ← get
  when (GLFW.keyCheck keyLayout k "ESC") $ liftIO $ GLFW.setWindowShouldClose window True
  when (GLFW.keyCheck keyLayout k "UPP") $ do
    let newcursor = (moveCursor 1 (cursor st) North)
    modify' $ \s → s { cursor = newcursor }
    logDebug $ "cursor: " ⧺ (show newcursor) ⧺ ", cam3d: " ⧺ (show (cam3d st))
    return ()
  when (GLFW.keyCheck keyLayout k "DWN") $ do
    let newcursor = (moveCursor 1 (cursor st) South)
    modify' $ \s → s { cursor = newcursor }
    logDebug $ "cursor: " ⧺ (show newcursor) ⧺ ", cam3d: " ⧺ (show (cam3d st))
    return ()
  when (GLFW.keyCheck keyLayout k "RGT") $ do
    let newcursor = (moveCursor 1 (cursor st) East)
    modify' $ \s → s { cursor = newcursor }
    logDebug $ "cursor: " ⧺ (show newcursor) ⧺ ", cam3d: " ⧺ (show (cam3d st))
    return ()
  when (GLFW.keyCheck keyLayout k "LFT") $ do
    let newcursor = (moveCursor 1 (cursor st) West)
    modify' $ \s → s { cursor = newcursor }
    logDebug $ "cursor: " ⧺ (show newcursor) ⧺ ", cam3d: " ⧺ (show (cam3d st))
    return ()
  when (GLFW.keyCheck keyLayout k "SH") $ do
    logDebug $ "shell"
    return ()
  when (GLFW.keyCheck keyLayout k "K") $ do
    let newcam3d  = (moveCursor 1.0 (cam3d st) North)
    modify' $ \s → s { cam3d  = newcam3d }
    logDebug $ "cursor: " ⧺ (show (cursor st)) ⧺ ", cam3d: " ⧺ (show newcam3d)
    return ()
  when (GLFW.keyCheck keyLayout k "C") $ do
    let newDS = (addTile (drawSt st))
    modify' $ \s → s { drawSt = newDS }
    logDebug $ "adding test tile..."
    return ()
  when (GLFW.keyCheck keyLayout k "J") $ do
    let newcam3d  = (moveCursor 1.0 (cam3d st) South)
    modify' $ \s → s { cam3d  = newcam3d }
    logDebug $ "cursor: " ⧺ (show (cursor st)) ⧺ ", cam3d: " ⧺ (show newcam3d)
    return ()
  when (GLFW.keyCheck keyLayout k "L") $ do
    let newcam3d  = (moveCursor 1.0 (cam3d st) East)
    modify' $ \s → s { cam3d  = newcam3d }
    logDebug $ "cursor: " ⧺ (show (cursor st)) ⧺ ", cam3d: " ⧺ (show newcam3d)
    return ()
  when (GLFW.keyCheck keyLayout k "H") $ do
    let newcam3d  = (moveCursor 1.0 (cam3d st) West)
    modify' $ \s → s { cam3d  = newcam3d }
    logDebug $ "cursor: " ⧺ (show (cursor st)) ⧺ ", cam3d: " ⧺ (show newcam3d)
    return ()

-- evaluates mouse input
evalMouse ∷ GLFW.Window → GLFW.MouseButton → GLFW.MouseButtonState → GLFW.ModifierKeys → Anamnesis ε σ ()
evalMouse win mb mbs mk = do
  st ← get
  when (mb == GLFW.mousebutt1) $ do
    (x,y) ← liftIO $ GLFW.getCursorPos win
    let (x',y') = convertPixels (x,y)
        windows = luaWindows (luaSt st)
        thisWindow = windows !! (currentWin st)
    linkTest (x',y') (windowLinks thisWindow)
    logDebug $ "mouse click 1 at x: " ⧺ (show x') ⧺ ", y: " ⧺ (show y') ⧺ " " ⧺ (show ( fst (linkPos ((windowLinks thisWindow) !! 0)))) ⧺ " " ⧺ (show ( snd (linkPos ((windowLinks thisWindow) !! 0))))

-- test the mouse click against every link
linkTest ∷ (Double,Double) → [WinLink] → Anamnesis ε σ ()
linkTest _     []           = return ()
linkTest (x,y) (link:links) = do
  case (posClose (buttWidth,buttHeight) (linkPos link) (x,y)) of
    True  → do
      logDebug $ "link"
      env ← ask
      let eventQ = envEventsChan env
      case (linkAction link) of
        "link" → do
          logDebug $ "following link to " ⧺ (linkLink link)
          liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdswitchWindow (linkLink link)) ""
        "action" → do
          logDebug $ "quitting..."
          window' ← gets windowSt
          case window' of
            Just window → liftIO $ GLFW.setWindowShouldClose window True
            Nothing → logWarn "no window to exit"
        action → logWarn $ "no known action " ⧺ action
    False → linkTest (x,y) links
  where (buttWidth,buttHeight) = linkSize link

convertPixels ∷ (Double,Double) → (Double,Double)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1080.0 / 2.0)) / 64.0)
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

posClose ∷ (Double,Double) → (Double,Double) → (Double,Double) → Bool
posClose (buttWidth,buttHeight) (x1,y1) (x2,y2)
  | ((abs(x1 - x2 + 1.5)) < buttWidth) && ((abs(y1 - y2)) < buttHeight) = True
  | otherwise = False
