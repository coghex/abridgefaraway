module Epiklesis.Lua where
-- the interface to the lua state
-- is instantiated.
import qualified Foreign.Lua as Lua
import Epiklesis.Data

initLua ∷ IO (LuaState)
initLua = do
  ls ← Lua.newstate
  return $ LuaState { luaState = ls }
