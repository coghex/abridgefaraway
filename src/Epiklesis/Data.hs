module Epiklesis.Data where
-- the interface to the lua state
-- is instantiated.
import qualified Foreign.Lua as Lua

data WinText = WinText { winPos ∷ (Float, Float)
                       , winBox ∷ Bool
                       , winText ∷ String } deriving (Show, Eq)

data Window = Window { winTitle ∷ String
                     , winBackground ∷ String
                     , windowText ∷ [WinText] } deriving (Show, Eq)
data LuaState = LuaState { luaState   ∷ Lua.State
                         , luaWindows ∷ [Window] }

-- possible lua commands
data LuaCmd = LuaCmdnewWindow Window | LuaCmdnewText String WinText | LuaCmdnewButton String WinText | LuaCmdswitchWindow String | LuaCmdNULL deriving (Show, Eq)
