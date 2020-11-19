{-# LANGUAGE Strict #-}
module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Elems
import Paracletus.Data
import qualified Data.ByteString.Char8 as BL
import qualified Foreign.Lua as Lua

-- empty shell
initShell ∷ Shell
initShell = Shell "$> " False 0 "" ""

-- executes lua command in state
evalShell ∷ LuaState → IO LuaState
evalShell ls = do
  let oldSh = luaShell ls
  (ret,outbuff) ← execShell (luaState ls) (shInpStr oldSh)
  let retstring = (shOutStr oldSh) ⧺ (shPrompt oldSh) ⧺ (shInpStr oldSh) ⧺ "\n" ⧺ (show ret) ⧺ " > " ⧺ outbuff ⧺ "\n"
      newSh = oldSh { shCursor = (shCursor oldSh) + 1
                    , shInpStr = ""
                    , shOutStr = retstring }
  return ls { luaShell = newSh }

execShell ∷ Lua.State → String → IO (Lua.Status,String)
execShell ls str = do
  Lua.runWith ls $ Lua.openlibs
  luaerror ← Lua.runWith ls $ Lua.loadstring $ BL.pack str
  _   ← Lua.runWith ls $ Lua.pcall 0 1 Nothing
  ret ← Lua.runWith ls $ Lua.tostring' $ Lua.nthFromBottom (-1)
  Lua.runWith ls $ Lua.pop $ Lua.nthFromBottom (-1)
  return $ (luaerror,(BL.unpack ret))

-- produce graphics tiles
genShell ∷ Shell → [GTile]
genShell sh = case (shOpen sh) of
                True  → (addTextBox posOffset size) ⧺ (addText (fst pos) pos' str)
                False → []
  where size = (20,8)
        posOffset = ((fst pos) - 1.0, (snd pos) + 0.5)
        pos = (-4.0,2.0)
        pos' = (-4.0,2.0+cursor)
        cursor = fromIntegral $ shCursor sh
        str = genShellStr sh--[shPrompt sh]
genShellStr ∷ Shell → String
genShellStr (Shell prompt _ _ strsin strsout) = strsout ⧺ prompt ⧺ strsin

removeShellString ∷ Shell → Shell
removeShellString sh = sh { shInpStr = init $ shInpStr sh }

addShellString ∷ Shell → String → Shell
addShellString sh str = sh { shInpStr = (shInpStr sh) ⧺ str }

-- reveals on screen
openSh ∷ Shell → Bool → Shell
openSh sh on = sh { shOpen = on }
