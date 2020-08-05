module Paracletus.Oblatum.GLFW
  ( module Graphics.UI.GLFW
  , keyCheck
  ) where
-- the input is defined for glfw, since
-- the library is just one giant unified
-- C library, we dont want to import
-- everything all at once, and this way
-- we can abstract to other window managers
import Prelude()
import UPrelude
import Control.Monad (when)
import Data.Char (toUpper)
import Graphics.UI.GLFW
import Paracletus.Oblatum.Data

-- mousebutton synonyms
--mousebutt1 ∷ GLFW.MouseButton
--mousebutt1 = GLFW.MouseButton'1
--mousebutt2 ∷ GLFW.MouseButton
--mousebutt2 = GLFW.MouseButton'2
--mousebutt3 ∷ GLFW.MouseButton
--mousebutt3 = GLFW.MouseButton'3
--mousebutt4 ∷ GLFW.MouseButton
--mousebutt4 = GLFW.MouseButton'4
--mousebutt5 ∷ GLFW.MouseButton
--mousebutt5 = GLFW.MouseButton'5
--mousebutt6 ∷ GLFW.MouseButton
--mousebutt6 = GLFW.MouseButton'6
--mousebutt7 ∷ GLFW.MouseButton
--mousebutt7 = GLFW.MouseButton'7
--mousebutt8 ∷ GLFW.MouseButton
--mousebutt8 = GLFW.MouseButton'8

-- translates keys from strings for ease of use
getGLFWKey ∷ String → Key
getGLFWKey "ESC" = Key'Escape
getGLFWKey "RET" = Key'Enter
getGLFWKey "DEL" = Key'Backspace
getGLFWKey "SPC" = Key'Space
getGLFWKey "LFT" = Key'Left
getGLFWKey "RGT" = Key'Right
getGLFWKey "UPP" = Key'Up
getGLFWKey "DWN" = Key'Down
getGLFWKey "C"   = Key'C
getGLFWKey "H"   = Key'H
getGLFWKey "J"   = Key'J
getGLFWKey "K"   = Key'K
getGLFWKey "L"   = Key'L
getGLFWKey _     = Key'Unknown

keyCheck ∷ KeyLayout → Key → String → Bool
keyCheck keyLayout k str = (k ≡ (getGLFWKey nk))
  where nk = applyKeyLayout keyLayout str

applyKeyLayout ∷ KeyLayout → String → String
applyKeyLayout keyLayout "ESC" = keyESC keyLayout
applyKeyLayout keyLayout "RET" = keyRET keyLayout
applyKeyLayout keyLayout "DEL" = keyDEL keyLayout
applyKeyLayout keyLayout "SPC" = keySPC keyLayout
applyKeyLayout keyLayout "C"   = keyC   keyLayout
applyKeyLayout keyLayout "R"   = keyR   keyLayout
applyKeyLayout keyLayout "`"   = keySH  keyLayout
applyKeyLayout keyLayout "H"   = keyH   keyLayout
applyKeyLayout keyLayout "J"   = keyJ   keyLayout
applyKeyLayout keyLayout "K"   = keyK   keyLayout
applyKeyLayout keyLayout "L"   = keyL   keyLayout
applyKeyLayout keyLayout "LFT" = keyLFT keyLayout
applyKeyLayout keyLayout "RGT" = keyRGT keyLayout
applyKeyLayout keyLayout "UPP" = keyUPP keyLayout
applyKeyLayout keyLayout "DWN" = keyDWN keyLayout

applyKeyLayout keyLayout _     = "NULL"

getKeyStr ∷ Key → IO (Maybe String)
getKeyStr k = getKeyName k 0

calcInpKey ∷ Key → ModifierKeys → IO String
calcInpKey k mk = do
  inp ← getKeyStr k
  case (inp) of
    Just str → return $ applyMod mk str
    Nothing  → return ""

applyMod ∷ ModifierKeys → String → String
applyMod mk str = if (modifierKeysShift mk) then map upcase str else str

upcase ∷ Char → Char
upcase '1'  = '!'
upcase '2'  = '@'
upcase '3'  = '#'
upcase '4'  = '$'
upcase '5'  = '%'
upcase '6'  = '^'
upcase '7'  = '&'
upcase '8'  = '*'
upcase '9'  = '('
upcase '0'  = ')'
upcase '-'  = '_'
upcase '='  = '+'
upcase '['  = '{'
upcase ']'  = '}'
upcase '\\' = '|'
upcase ';'  = ':'
upcase '\'' = '"'
upcase ','  = '<'
upcase '.'  = '>'
upcase '/'  = '?'
upcase c    = toUpper c
