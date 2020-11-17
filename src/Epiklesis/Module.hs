module Epiklesis.Module where
-- loading of a lua module of multiple
-- types, different modules require
-- different required fields or functions
-- some module types load in extra functions
import Prelude()
import UPrelude
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Artos.Var
import Artos.Queue
import Epiklesis.Data

-- loads lua functions for a module
loadModule ∷ Env → State → ModuleType → String → IO (Module)
loadModule env st ModuleUser fp = do
  let ls = luaState $ luaSt st
  _ ← Lua.runWith ls $ do
    Lua.openlibs
    _ ← Lua.dofile fp
    return ()
  let eventQ = envEventsChan env
  atomically $ writeQueue eventQ $ EventLoaded
  return (Module fp ModuleUser)
loadModule env st ModuleGame fp = do
  let ls = luaState $ luaSt st
  _ ← Lua.runWith ls $ do
    Lua.openlibs
    Lua.registerHaskellFunction "findScreenCursor" (hsFindScreenCursor env)
    _ ← Lua.dofile fp
    return ()
  let eventQ = envEventsChan env
  atomically $ writeQueue eventQ $ EventLoaded
  return ()
  return (Module fp ModuleGame)

hsFindScreenCursor ∷ Env → String → Lua.Lua ()
hsFindScreenCursor env str = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaFind LFScreenCursor)

logLuaError ∷ Env -> String → IO ()
logLuaError env str = do
  let eventQ = envEventsChan env
  atomically $ writeQueue eventQ $ EventLua (LuaError str)
