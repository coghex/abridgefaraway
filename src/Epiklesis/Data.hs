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
                         , luaLastWin ∷ Int
                         , luaWindows ∷ [Window] }

data WinElem = WinElemText  { textPos ∷ (Double,Double)
                            , textBox ∷ Bool
                            , textStr ∷ String }
             | WinElemBack  { backFP  ∷ String }
             | WinElemLink  { linkPos ∷ (Double,Double)
                            , linkBox ∷ (Double,Double)
                            , linkAct ∷ LinkAction }
             | WinElemWorld { worldPars ∷ WorldParams
                            , worldData ∷ WorldData
                            , worldTexs ∷ [String] }
             | WinElemNULL deriving (Show, Eq)
-- define an order so sorting is consistent,
-- some equalities are more or less arbitrary
instance Ord WinElem where
  --these compare against themselves
  compare (WinElemText tp1 _ _) (WinElemText tp2 _ _) = EQ
  compare (WinElemLink lp1 _ _) (WinElemLink lp2 _ _) = EQ
  compare (WinElemBack _) (WinElemBack _)             = EQ
  compare (WinElemWorld _ _ _) (WinElemWorld _ _ _)   = EQ
  compare (WinElemNULL) (WinElemNULL)                 = EQ
  -- this is where the real definition is
  compare (WinElemBack _) (WinElemWorld _ _ _)        = LT
  compare (WinElemBack _) (WinElemLink _ _ _)         = GT
  compare (WinElemBack _) (WinElemNULL)               = GT
  compare (WinElemWorld _ _ _) (WinElemBack _)        = GT
  compare (WinElemWorld _ _ _) (WinElemLink _ _ _)    = GT
  compare (WinElemWorld _ _ _) (WinElemNULL)          = GT
  -- these help cover all cases where we dont
  -- even have textures to compare
  compare (WinElemLink _ _ _) _ = LT
  compare _ (WinElemLink _ _ _) = GT
  compare (WinElemText _ _ _) _ = GT
  compare _ (WinElemText _ _ _) = LT
  compare (WinElemNULL) _       = LT
  compare _ (WinElemNULL)       = GT

-- possible actions when links are clicked
data LinkAction = LinkExit | LinkBack | LinkLink String | LinkNULL deriving (Show, Eq)

-- possible lua commands, including errors
data LuaCmd = LuaCmdnewWindow Window
            | LuaCmdnewElem String WinElem
            | LuaCmdswitchWindow String
            | LuaError String
            | LuaCmdNULL deriving (Show, Eq)

-- world parameters help generate world
data WorldParams = WorldParams { wpZSize ∷ (Int,Int)
                               , wpSSize ∷ (Int,Int)
                               , wpNTexs ∷ Int } deriving (Show, Eq)

-- camera stored with each world
data WorldData = WorldData { wdCam   ∷ (Float,Float)
                           , wdCSize ∷ (Int,Int)
                           , wdZones ∷ [Zone] } deriving (Show, Eq)
