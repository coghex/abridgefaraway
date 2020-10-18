module Epiklesis.Data where
-- the interface to the lua state
-- is instantiated.
import qualified Foreign.Lua as Lua

data Window = Window { winTitle ∷ String
                     , winBackground ∷ String } deriving (Show, Eq)
data LuaState = LuaState { luaState   ∷ Lua.State
                         , luaWindows ∷ [Window] }

-- possible lua commands
data LuaCmd = LuaCmdnewWindow Window | LuaCmdNULL deriving (Show, Eq)
