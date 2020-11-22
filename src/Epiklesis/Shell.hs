{-# LANGUAGE Strict #-}
module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Anamnesis.Data
import Epiklesis.Data
import Epiklesis.Elems
import Epiklesis.Module
import Paracletus.Data
import qualified Data.ByteString.Char8 as BL
import qualified Foreign.Lua as Lua

-- empty shell
initShell ∷ Shell
initShell = Shell "$> " False 0 "" ""

-- executes lua command in state
evalShell ∷ Env → LuaState → IO LuaState
evalShell env ls = do
  let oldSh = luaShell ls
  (ret,outbuff) ← execShell env (luaState ls) (shInpStr oldSh)
  let retstring = (shOutStr oldSh) ⧺ (shPrompt oldSh) ⧺ (shInpStr oldSh) ⧺ "\n" ⧺ (show ret) ⧺ " > " ⧺ outbuff ⧺ "\n"
      newSh = oldSh { shInpStr = ""
                    , shOutStr = retstring }
  return ls { luaShell = newSh }

execShell ∷ Env → Lua.State → String → IO (Lua.Status,String)
execShell env ls str = do
  Lua.runWith ls $ do
    Lua.openlibs
    loadModuleFunctions env
  luaerror ← Lua.runWith ls $ Lua.loadstring $ BL.pack str
  _   ← Lua.runWith ls $ Lua.pcall 0 1 Nothing
  ret ← Lua.runWith ls $ Lua.tostring' $ Lua.nthFromBottom (-1)
  Lua.runWith ls $ Lua.pop $ Lua.nthFromBottom (-1)
  return $ (luaerror,(BL.unpack ret))

-- produce graphics tiles
genShell ∷ Shell → [GTile]
genShell sh = case (shOpen sh) of
                True  → (addTextBox posOffset size) ⧺ (addText False (fst pos) pos' str)
                False → []
  where size = (32,18)
        posOffset = ((fst pos) - 1.0, (snd pos) + 0.5)
        pos = (-7.0,4)
        pos' = (-7.0,4)
        str = genShellStr sh--[shPrompt sh]
genShellStr ∷ Shell → String
genShellStr (Shell prompt _ _ strsin strsout)
  | (height > 8) = shortret
  | otherwise    = retstring
  where height = length $ filter (≡ '\n') retstring
        retstring = strsout ⧺ prompt ⧺ strsin
        shortret = flattenWith '\n' $ drop (height - 8) (splitOn "\n" retstring)
        flattenWith ∷ Char → [String] → String
        flattenWith _  []         = ""
        flattenWith ch (str:strs) = str ⧺ [ch] ⧺ flattenWith ch strs

removeShellString ∷ Shell → Shell
removeShellString sh = sh { shInpStr = init $ shInpStr sh }

addShellString ∷ Shell → String → Shell
addShellString sh str = sh { shInpStr = (shInpStr sh) ⧺ str }

-- reveals on screen
openSh ∷ Shell → Bool → Shell
openSh sh on = sh { shOpen = on }
