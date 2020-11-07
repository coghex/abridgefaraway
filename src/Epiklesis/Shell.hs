module Epiklesis.Shell where
-- a shell for lua commands is defined
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as BL
import qualified Foreign.Lua as Lua
import Anamnesis
import Anamnesis.Data
import Anamnesis.Util
import Epiklesis.Data
import qualified Paracletus.Oblatum.GLFW as GLFW

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
      newsh = oldsh { shString = (shString oldsh) ⧺ retstring ⧺ "\n$> " }
  return $ ds { dsShell = newsh }

-- executes a string in the lua state
execShell :: Lua.State -> String -> IO (Lua.Status,String)
execShell ls str = do
  Lua.runWith ls $ Lua.openlibs
  error <- Lua.runWith ls $ Lua.loadstring $ BL.pack str
  res <- Lua.runWith ls $ Lua.pcall 0 1 Nothing
  ret <- Lua.runWith ls $ Lua.tostring' $ Lua.nthFromBottom (-1)
  Lua.runWith ls $ Lua.pop $ Lua.nthFromBottom (-1)
  return $ (error,(BL.unpack ret))

addShellString ∷ DrawState → String → DrawState
addShellString ds str = updateShell ds newsh
  where newsh = (dsShell ds) { shString = (shString (dsShell ds)) ⧺ str }

removeShellString ∷ DrawState → DrawState
removeShellString ds = updateShell ds newsh
  where newsh = (dsShell ds) { shString = init (shString (dsShell ds)) }

updateShell ∷ DrawState → Shell → DrawState
updateShell ds sh = ds { dsShell = sh }
