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
evalMouse win mb mbs _  = return ()

convertPixels ∷ ∀ a. (Fractional a, Num a) ⇒ (a,a) → (a,a)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1280.0 / 2.0)) / 64.0)
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

posClose ∷ (Double,Double) → (Double,Double) → (Double,Double) → Bool
posClose (buttWidth,buttHeight) (x1,y1) (x2,y2)
  | ((abs(x1 - x2)) < buttWidth) && ((abs(y1 - y2)) < buttHeight) = True
  | otherwise = False
