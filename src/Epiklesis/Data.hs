module Epiklesis.Data where
-- the interface to the lua state
-- is instantiated.
import qualified Foreign.Lua as Lua

data LuaState = LuaState { luaState ∷ Lua.State }
