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
  when ((mb ≡ GLFW.mousebutt1) ∧ ((winType thisWin) ≡ WinTypeMenu)) $ do
    if (mbs ≡ GLFW.MouseButtonState'Pressed) then do
      pos' ← liftIO $ GLFW.getCursorPos win
      let pos = convertPixels pos'
      linkTest pos (winElems thisWin)
      -- save cache as raw pixels, not units
      let newIS = oldIS { mouse1      = True
                        , mouse1Cache = (realToFrac (fst pos'), realToFrac (snd pos')) }
      modify' $ \s → s { inputState = newIS }
    else if (mbs ≡ GLFW.MouseButtonState'Released) then do
      let newIS = oldIS { mouse1 = False }
      modify' $ \s → s { inputState = newIS }
    else return ()

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
        LinkNULL → logError "linkNULL clicked"
      linkTest (x,y) links
    False → linkTest (x,y) links

isElemLink ∷ WinElem → Bool
isElemLink (WinElemLink _ _ _) = True
isElemLink (WinElemText _ _ _) = False
isElemLink (WinElemBack _ )    = False
isElemLink (WinElemNULL)       = False
