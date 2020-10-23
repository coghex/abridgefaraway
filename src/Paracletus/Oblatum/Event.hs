{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Event where
-- key input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify')
import Anamnesis
import Anamnesis.Data
import Anamnesis.Draw
import Anamnesis.Util
import Anamnesis.Map
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
  state ← get
  let thisWinText = filter isButton $ windowText $ (luaWindows (luaSt state)) !! (currentWin state)
      isButton ∷ WinText → Bool
      isButton (WinText _ False _) = False
      isButton (WinText _ True  _) = True
      testButt ∷ (Double,Double) → WinText → Bool
      testButt clickpos (WinText pos _ _)
        | posClose pos clickpos = True
        | otherwise = False
      posClose ∷ (Double,Double) → (Double,Double) → Bool
      posClose (x1,y1) (x2,y2)
        | ((abs(x1-x2)) < buttWidth) && ((abs(y1-y2)) < buttHeight) = True
        | otherwise = False
      buttWidth = 4.0
      buttHeight = 1.0
  when (mb == GLFW.mousebutt1) $ do
    (x,y) ← liftIO $ GLFW.getCursorPos win
    let (x',y') = convertPixels (x,y)
    let bools = map (testButt (x',y')) thisWinText
    logDebug $ "mouse click 1 at x: " ⧺ (show x') ⧺ ", y: " ⧺ (show y') ⧺ (show bools)

convertPixels ∷ (Double,Double) → (Double,Double)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1080.0 / 2.0)) / 64.0) - 1.0
        y' = ((y - ( 720.0 / 2.0)) / 64.0) + 0.5
