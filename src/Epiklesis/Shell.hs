module Epiklesis.Shell where
-- a shell for lua commands is defined
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as BL
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Epiklesis.Data

initShell ∷ DrawState → DrawState
initShell ds = ds { dsShell = Shell "$> " (-6,4) (24,8) }

-- evaluates the shell, exceutes the command
-- returns the result in a new draw state
evalShell ∷ DrawState → Lua.State → IO (DrawState)
evalShell ds ls = do
  let execCommand = formatCmd (shString oldsh)
      oldsh = dsShell ds
      formatCmd ∷ String → String
      formatCmd s = tail $ tail $ tail $ last $ splitOn ['\n'] s
  (ret,outbuff) ← execShell ls execCommand
  let retstring = "\n" ⧺ (show ret) ⧺ " > " ⧺ outbuff
      newsh = Shell ((shString oldsh) ⧺ retstring ⧺ "\n$> ") (shPos oldsh) (shSize oldsh)
  return $ ds { dsShell = newsh }

-- executes a string in the lua state
execShell :: Lua.State -> String -> IO (Lua.Status,String)
execShell ls str = do
  Lua.runWith ls $ Lua.openlibs
  luaerror <- Lua.runWith ls $ Lua.loadstring $ BL.pack str
  _   <- Lua.runWith ls $ Lua.pcall 0 1 Nothing
  ret <- Lua.runWith ls $ Lua.tostring' $ Lua.nthFromBottom (-1)
  Lua.runWith ls $ Lua.pop $ Lua.nthFromBottom (-1)
  return $ (luaerror,(BL.unpack ret))

addShellString ∷ DrawState → String → DrawState
addShellString ds str = updateShell ds newsh
  where newsh = Shell ((shString (dsShell ds)) ⧺ str) (shPos oldsh) (shSize oldsh)
        oldsh = dsShell ds

removeShellString ∷ DrawState → DrawState
removeShellString ds = updateShell ds newsh
  where newsh = Shell (init (shString (dsShell ds))) (shPos oldsh) (shSize oldsh)
        oldsh = dsShell ds

updateShell ∷ DrawState → Shell → DrawState
updateShell ds sh = ds { dsShell = sh }
