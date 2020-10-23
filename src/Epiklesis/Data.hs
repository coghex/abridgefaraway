module Epiklesis.Data where
-- the interface to the lua state
-- is instantiated.
import qualified Foreign.Lua as Lua

-- a generic set of text, box bool will
-- draw a box behind it, pos in game coords
data WinText = WinText { winPos ∷ (Double,Double)
                       , winBox ∷ Bool
                       , winText ∷ String } deriving (Show, Eq)

-- a generic tile, use to display a single sprite
data WinTile = WinTile { winTilePos ∷ (Double,Double)
                       , winTileTex ∷ String } deriving (Show, Eq)

-- data struct of what can be on a window,
-- every window switch entails a full reload
-- of textures and swapchain recreation
data Window = Window { winTitle ∷ String
                     , winBackground ∷ String
                     , windowText ∷ [WinText]
                     , windowTiles ∷ [WinTile] } deriving (Show, Eq)

-- the windows defined by the lua fies
data LuaState = LuaState { luaState   ∷ Lua.State
                         , luaWindows ∷ [Window] }

-- possible lua commands
data LuaCmd = LuaCmdnewWindow Window
            | LuaCmdnewText String WinText
            | LuaCmdnewButton String WinText
            | LuaCmdswitchWindow String
            | LuaCmdnewTile String WinTile
            | LuaCmdNULL deriving (Show, Eq)
