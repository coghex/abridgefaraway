{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Event where
-- key input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify')
import Anamnesis
import Anamnesis.Data
import Anamnesis.Map
import Paracletus.Oblatum.Data (KeyLayout)
import qualified Paracletus.Oblatum.GLFW as GLFW
-- user key strings from getKey function
evalKey ∷ GLFW.Window → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → KeyLayout → Anamnesis ε σ ()
evalKey window k _  _  keyLayout = do
  st ← get
  when (GLFW.keyCheck keyLayout k "ESC") $ liftIO $ GLFW.setWindowShouldClose window True
  when (GLFW.keyCheck keyLayout k "UPP") $ do
    modify' $ \s → s { cursor = (moveCursor 1 (cursor st) North) }
    return ()
  when (GLFW.keyCheck keyLayout k "DWN") $ do
    modify' $ \s → s { cursor = (moveCursor 1 (cursor st) South) }
    return ()
  when (GLFW.keyCheck keyLayout k "RGT") $ do
    modify' $ \s → s { cursor = (moveCursor 1 (cursor st) East) }
    return ()
  when (GLFW.keyCheck keyLayout k "LFT") $ do
    modify' $ \s → s { cursor = (moveCursor 1 (cursor st) West) }
    return ()
