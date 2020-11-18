{-# LANGUAGE Strict #-}
module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Elems
import Paracletus.Data

-- empty shell
initShell ∷ Shell
initShell = Shell "\n$> " False "" ""

-- produce graphics tiles
genShell ∷ Shell → [GTile]
genShell sh = case (shOpen sh) of
                True  → (addTextBox posOffset size) ⧺ addText pos str
                False → []
  where size = (20,8)
        posOffset = ((fst pos) - 1.0, (snd pos) + 0.5)
        pos = (-4.0,2.0)
        str = genShellStr sh--[shPrompt sh]
genShellStr ∷ Shell → String
genShellStr (Shell prompt _ strsin strsout) = prompt ⧺ strsin ⧺ strsout

removeShellString ∷ Shell → Shell
removeShellString sh = sh { shInpStr = init $ shInpStr sh }

addShellString ∷ ShellIO → Shell → String → Shell
addShellString ShellInp sh str = sh { shInpStr = (shInpStr sh) ⧺ str }
addShellString ShellOut sh str = sh { shOutStr = (shOutStr sh) ⧺ str }

-- reveals on screen
openSh ∷ Shell → Bool → Shell
openSh sh on = sh { shOpen = on }
