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

removeShellString ∷ DrawState → DrawState
removeShellString ds = updateShell ds newsh
  where newsh = (dsShell ds) { shString = init (shString (dsShell ds)) }

updateShell ∷ DrawState → Shell → DrawState
updateShell ds sh = ds { dsShell = sh }
