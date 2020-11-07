module Epiklesis.Shell where
-- a shell for lua commands is defined
import Prelude()
import UPrelude
import Anamnesis.Data
import Anamnesis.Util
import Epiklesis.Data
import qualified Paracletus.Oblatum.GLFW as GLFW

initShell ∷ DrawState → DrawState
initShell ds = ds { dsShell = Shell "$> " (-6,4) (24,8) }

addShellString ∷ DrawState → String → DrawState
addShellString ds str = updateShell ds newsh
  where newsh = (dsShell ds) { shString = (shString (dsShell ds)) ⧺ str }

updateShell ∷ DrawState → Shell → DrawState
updateShell ds sh = ds { dsShell = sh }

evalShell ∷ GLFW.Window → Shell → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → GLFW.KeyLayout → Shell
evalShell win sh k ks mk kl
  | (ks == GLFW.KeyState'Pressed) = if (GLFW.keyCheck kl k "SH") then ShellNULL else sh
  | otherwise = sh
