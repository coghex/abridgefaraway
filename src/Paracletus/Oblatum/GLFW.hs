module Paracletus.Oblatum.GLFW
  ( module Graphics.UI.GLFW
  , KeyLayout (..)
  , keyCheck
  , calcInpKey
  , mousebutt1
  , mousebutt2
  , mousebutt3
  ) where
-- the input is defined for glfw, since
-- the library is just one giant unified
-- C library, we dont want to import
-- everything all at once, and this way
-- we can abstract to other window managers
import Prelude()
import UPrelude
import Data.Char (toUpper,toLower)
import Graphics.UI.GLFW
import Paracletus.Oblatum.Data

-- mousebutton synonyms
mousebutt1 ∷ MouseButton
mousebutt1 = MouseButton'1
mousebutt2 ∷ MouseButton
mousebutt2 = MouseButton'2
mousebutt3 ∷ MouseButton
mousebutt3 = MouseButton'3
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
getGLFWKey "TAB" = Key'Tab
getGLFWKey "LFT" = Key'Left
getGLFWKey "RGT" = Key'Right
getGLFWKey "UPP" = Key'Up
getGLFWKey "DWN" = Key'Down
getGLFWKey "SH"  = Key'GraveAccent
getGLFWKey "C"   = Key'C
getGLFWKey "H"   = Key'H
getGLFWKey "J"   = Key'J
getGLFWKey "K"   = Key'K
getGLFWKey "L"   = Key'L
getGLFWKey "W"   = Key'W
getGLFWKey "A"   = Key'A
getGLFWKey "S"   = Key'S
getGLFWKey "D"   = Key'D
getGLFWKey _     = Key'Unknown

keyCheck ∷ Bool → KeyLayout → Key → String → Bool
keyCheck cap keyLayout k str
  | cap       = False
  | otherwise = (k ≡ (getGLFWKey nk))
  where nk = applyKeyLayout keyLayout str

applyKeyLayout ∷ KeyLayout → String → String
applyKeyLayout keyLayout "ESC" = klEsc keyLayout
applyKeyLayout keyLayout "RET" = klRet keyLayout
applyKeyLayout keyLayout "DEL" = klDel keyLayout
applyKeyLayout keyLayout "SPC" = klSpc keyLayout
applyKeyLayout keyLayout "TAB" = klTab keyLayout
applyKeyLayout keyLayout "LFT" = klLft keyLayout
applyKeyLayout keyLayout "RGT" = klRgt keyLayout
applyKeyLayout keyLayout "UP"  = klUp  keyLayout
applyKeyLayout keyLayout "DWN" = klDwn keyLayout
applyKeyLayout keyLayout "SH"  = klSh  keyLayout
applyKeyLayout keyLayout "UPA" = "UPP"
applyKeyLayout keyLayout "DNA" = "DWN"
applyKeyLayout _         "H"   = "H"
applyKeyLayout _         "J"   = "J"
applyKeyLayout _         "K"   = "K"
applyKeyLayout _         "L"   = "L"
applyKeyLayout _         "C"   = "C"
applyKeyLayout _         _     = "NULL"

getKeyStr ∷ Key → IO (Maybe String)
getKeyStr k = getKeyName k 0

calcInpKey ∷ Key → ModifierKeys → IO String
calcInpKey k mk = do
  inp ← getKeyStr k
  case (inp) of
    Just str → return $ applyMod mk str
    Nothing  → return ""

applyMod ∷ ModifierKeys → String → String
applyMod mk str = if (modifierKeysShift mk) then map upcase str else map downcase str

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
upcase '`'  = '~'
upcase c    = toUpper c
downcase ∷ Char → Char
downcase '!' = '1'
downcase '@' = '2'
downcase '#' = '3'
downcase '$' = '4'
downcase '%' = '5'
downcase '^' = '6'
downcase '&' = '7'
downcase '*' = '8'
downcase '(' = '9'
downcase ')' = '0'
downcase '_' = '-'
downcase '+' = '='
downcase '{' = '['
downcase '}' = ']'
downcase '|' = '\\'
downcase ':' = ';'
downcase '"' = '\''
downcase '<' = ','
downcase '>' = '.'
downcase '?' = '/'
downcase '~' = '`'
downcase c  = toLower c
