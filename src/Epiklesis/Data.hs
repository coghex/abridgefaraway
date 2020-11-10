{-# LANGUAGE DeriveDataTypeable #-}
module Epiklesis.Data where
-- the interface to the lua state
-- is instantiated.
import Epiklesis.World
import Data.Typeable
import Data.Data
import qualified Foreign.Lua.Types.Peekable as Lua.Peekable
import qualified Foreign.Lua.Types.Pushable as Lua.Pushable
import qualified Foreign.Lua as Lua

-- window types define behavior
data WinType = WinTypeMenu | WinTypeGame | WinTypeNULL deriving (Show, Eq)

-- windows contain their own camera
data Window = Window { winTitle  ∷ String
                     , winType   ∷ WinType
                     , winCursor ∷ (Float,Float,Float)
                     , winElems  ∷ [WinElem]
                     } deriving (Show, Eq)

-- the windows defined by the lua fies,
-- they are made up of elements which
-- can be any number of things, windows
-- are classified into types to provide
-- different functionality to each window
data LuaState = LuaState { luaState   ∷ Lua.State
                         , luaCurrWin ∷ Int
                         , luaWindows ∷ [Window] }

data WinElem = WinElemText { textPos ∷ (Double,Double)
                           , textBox ∷ Bool
                           , textStr ∷ String }
             | WinElemBack { backFP  ∷ String }
             | WinElemLink { linkPos ∷ (Double,Double)
                           , linkBox ∷ (Double,Double)
                           , linkAct ∷ LinkAction }
             | WinElemNULL deriving (Show, Eq)

-- possible actions when links are clicked
data LinkAction = LinkExit | LinkLink String | LinkNULL deriving (Show, Eq)

-- possible lua commands, including errors
data LuaCmd = LuaCmdnewWindow Window
            | LuaCmdnewElem String WinElem
            | LuaCmdswitchWindow String
            | LuaError String
            | LuaCmdNULL deriving (Show, Eq)
