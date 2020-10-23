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

-- links will change windows on clicks
data WinLink = WinLink { linkPos  ∷ (Double,Double)
                       , linkSize ∷ (Double,Double)
                       , linkAction ∷ String
                       , linkLink ∷ String } deriving (Show, Eq)

-- a generic menu of possible elements
data WinMenu = WinMenu { menuName  ∷ String
                       , menuPos   ∷ (Double,Double)
                       , menuElems ∷ [WinElem] } deriving (Show, Eq)

-- a element can be many things
data WinElem = WinElemText String | WinElemNULL deriving (Show, Eq)

-- data struct of what can be on a window,
-- every window switch entails a full reload
-- of textures and swapchain recreation
data Window = Window { winTitle      ∷ String
                     , winBackground ∷ String
                     , windowText    ∷ [WinText]
                     , windowLinks   ∷ [WinLink]
                     , windowTiles   ∷ [WinTile]
                     , windowMenus   ∷ [WinMenu] } deriving (Show, Eq)

-- the windows defined by the lua fies
data LuaState = LuaState { luaState   ∷ Lua.State
                         , luaWindows ∷ [Window] }

-- possible lua commands, including errors
data LuaCmd = LuaCmdnewWindow Window
            | LuaCmdnewText String WinText
            | LuaCmdnewButton String WinText String
            | LuaCmdswitchWindow String
            | LuaCmdnewLink String WinLink
            | LuaCmdnewTile String WinTile
            | LuaCmdnewMenu String WinMenu
            | LuaError String
            | LuaCmdNULL deriving (Show, Eq)
