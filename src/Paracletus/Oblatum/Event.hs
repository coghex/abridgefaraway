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
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Paracletus.Draw
import Paracletus.Oblatum.Data
import qualified Paracletus.Oblatum.GLFW as GLFW
-- user key strings from getKey function
evalKey ∷ GLFW.Window → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → GLFW.KeyLayout → Anamnesis ε σ ()
evalKey window k ks mk keyLayout = do
  when (GLFW.keyCheck False keyLayout k "ESC") $ liftIO $ GLFW.setWindowShouldClose window True

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
      logDebug $ "mouse 3 click"
      pos' ← liftIO $ GLFW.getCursorPos win
      let pos = ((realToFrac (fst pos')),(realToFrac (snd pos')))
          newIS = oldIS { mouse3 = True
                        , mouse3Cache = pos }
      modify' $ \s → s { inputState = newIS }
      --logDebug $ "mouse 3 click at: " ⧺ (show pos)
  -- mouse button 3 release
    else if ((mbs ≡ GLFW.MouseButtonState'Released) ∧ ((mouse3 oldIS) ≡ True)) then do
      logDebug $ "mouse 3 unclick"
      if ((winType thisWin) ≡ WinTypeGame) then do
        case (findWorldData thisWin) of
          Just (_,wd) → do
            let newIS = oldIS { mouse3 = False }
                sc = ((wdCam wd),(wdCSize wd))
            liftIO $ reloadScreenCursor env sc
            modify' $ \s → s { inputState = newIS }
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
isElemLink (WinElemText _ _ _) = False
isElemLink (WinElemBack _ )    = False
isElemLink (WinElemNULL)       = False

moveCamWithMouse ∷ Anamnesis ε σ ()
moveCamWithMouse = do
  st ← get
  let win' = windowSt st
  case win' of
    Just win → do
      let currWin = (luaWindows ls) ‼ (luaCurrWin ls)
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
                newDS = loadDrawState newLS
            modify' $ \s → s { drawSt = newDS
                            , luaSt = newLS
                            , inputState = newIS }
          Nothing → return ()
      else return ()
    Nothing → return ()

moveScreenCursor ∷ (Float,Float,Float) → (Float,Float)
moveScreenCursor (x,y,z) = (-0.05*x,-0.05*y)
