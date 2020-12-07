{-# LANGUAGE DeriveDataTypeable #-}
module Epiklesis.Data where
-- the interface to the lua state
-- is instantiated.
import Epiklesis.World
import Paracletus.Data
import Paracletus.Oblatum.Data
import qualified Foreign.Lua as Lua

-- window types define behavior
data WinType = WinTypeMenu | WinTypeGame | WinTypeNULL deriving (Show, Eq)

-- windows contain their own camera
data Window = Window { winTitle  ∷ String
                     , winType   ∷ WinType
                     , winCursor ∷ (Float,Float,Float)
                     , winElems  ∷ [WinElem]
                     , winCache  ∷ [WinElemCache]
                     }

-- lua shell executes commands in global state
data Shell = Shell { shPrompt ∷ String
                   , shOpen   ∷ Bool
                   , shCursor ∷ Int
                   , shInpStr ∷ String
                   , shOutStr ∷ String } deriving (Show, Eq)

-- the windows defined by the lua fies,
-- they are made up of elements which
-- can be any number of things, windows
-- are classified into types to provide
-- different functionality to each window
data LuaState = LuaState { luaState   ∷ Lua.State
                         , luaFPS     ∷ Maybe Int
                         , luaConfig  ∷ LuaConfig
                         , luaCurrWin ∷ Int
                         , luaLastWin ∷ Int
                         , luaNDefTex ∷ Int
                         , luaShell   ∷ Shell
                         , luaModules ∷ [Module]
                         , luaWindows ∷ [Window] }

data LuaConfig = LuaConfig { lcKeyLayout ∷ KeyLayout }

data WinElem = WinElemText  { textPos ∷ (Double,Double)
                            , textBox ∷ Bool
                            , textStr ∷ String }
             | WinElemMenu  { menuName ∷ String
                            , menuPos  ∷ (Double,Double)
                            , menuBits ∷ [MenuBit] }
             | WinElemBack  { backFP  ∷ String }
             | WinElemLink  { linkPos ∷ (Double,Double)
                            , linkBox ∷ (Double,Double)
                            , linkAct ∷ LinkAction }
             | WinElemWorld { worldPars ∷ WorldParams
                            , worldData ∷ WorldData
                            , worldTexs ∷ [String] }
             | WinElemDyn   { dynType ∷ DynType
                            , dynData ∷ [DynData] }
             | WinElemNULL deriving (Show, Eq)
-- define an order so sorting is consistent,
-- some equalities are more or less arbitrary
instance Ord WinElem where
  --these compare against themselves
  compare (WinElemText _ _ _) (WinElemText _ _ _)     = EQ
  compare (WinElemMenu _ _ _) (WinElemMenu _ _ _)             = EQ
  compare (WinElemLink _ _ _) (WinElemLink _ _ _)     = EQ
  compare (WinElemBack _) (WinElemBack _)             = EQ
  compare (WinElemWorld _ _ _) (WinElemWorld _ _ _)   = EQ
  compare (WinElemDyn _ _) (WinElemDyn _ _)           = EQ
  compare (WinElemNULL) (WinElemNULL)                 = EQ
  -- this is where the real definition is
  compare (WinElemBack _) (WinElemWorld _ _ _)        = LT
  compare (WinElemBack _) (WinElemMenu _ _ _)         = GT
  compare (WinElemBack _) (WinElemLink _ _ _)         = GT
  compare (WinElemBack _) (WinElemNULL)               = GT
  compare (WinElemBack _) (WinElemDyn _ _)            = LT
  compare (WinElemWorld _ _ _) (WinElemBack _)        = GT
  compare (WinElemWorld _ _ _) (WinElemMenu _ _ _)    = GT
  compare (WinElemWorld _ _ _) (WinElemLink _ _ _)    = GT
  compare (WinElemWorld _ _ _) (WinElemDyn _ _)       = LT
  compare (WinElemWorld _ _ _) (WinElemNULL)          = GT
  compare (WinElemDyn _ _) (WinElemBack _)            = GT
  compare (WinElemDyn _ _) (WinElemWorld _ _ _)       = GT
  compare (WinElemDyn _ _) (WinElemMenu _ _ _)        = GT
  compare (WinElemDyn _ _) (WinElemLink _ _ _)        = GT
  compare (WinElemDyn _ _) (WinElemNULL)              = GT
  compare (WinElemMenu _ _ _) (WinElemBack _)         = GT
  compare (WinElemMenu _ _ _) (WinElemDyn _ _)        = LT
  compare (WinElemMenu _ _ _) (WinElemWorld _ _ _)    = GT
  compare (WinElemMenu _ _ _) (WinElemLink _ _ _)     = GT
  compare (WinElemMenu _ _ _) (WinElemNULL)           = GT
  compare (WinElemLink _ _ _) _ = LT
  compare _ (WinElemLink _ _ _) = GT
  compare (WinElemText _ _ _) _ = GT
  compare _ (WinElemText _ _ _) = LT
  compare (WinElemNULL) _       = LT

-- the cache allows instant gtile
-- calculation when desired
data WinElemCache = WECached [GTile]
                  | WEUncached
                  | WECacheNULL

-- possible actions when links are clicked
data LinkAction = LinkExit | LinkBack | LinkLink String | LinkNULL deriving (Show, Eq)

-- possible lua commands, including errors
data LuaCmd = LuaCmdnewWindow Window
            | LuaCmdnewElem String WinElem WinElemCache
            | LuaCmdnewMenuBit String String MenuBit
            | LuaCmdswitchWindow String
            | LuaCmdloadModule String
            | LuaCmdtoggleFPS
            | LuaFind LFQuery
            | LuaError String
            | LuaCmdNULL-- deriving (Show, Eq)
-- possible queryable items in the state
data LFQuery = LFScreenCursor | LFNULL deriving (Show, Eq)

data Module = Module { modFP   ∷ String
                     , modType ∷ ModuleType }
data ModuleType = ModuleGame | ModuleUser

-- possible menu elements
data MenuBit = MenuText { menuText ∷ String }
             | MenuSlider { sliderText ∷ String
                          , sliderRange ∷ (Int,Int) }
             | MenuNULL deriving (Show, Eq)

-- world parameters help generate world
data WorldParams = WorldParams { wpZSize ∷ (Int,Int)
                               , wpSSize ∷ (Int,Int)
                               --, wpNCont ∷ Int
                               --, wpConts ∷ [(Int,Int)]
                               --, wpSeeds ∷ [[(Int,Int)]]
                               --, wpRands ∷ [[(Int,Int)]]
                               --, wpSizes ∷ [Int]
                               --, wpTypes ∷ [Biome]
                               , wpNTexs ∷ Int } deriving (Show, Eq)

data Biome = BSea | BShallows | BDeeps | BValley | BCrags | BPlains | BFields | BWastes | BSteeps | BPeaks | BNULL deriving (Show, Eq)

-- camera stored with each world
data WorldData = WorldData { wdCam   ∷ (Float,Float)
                           , wdCSize ∷ (Int,Int)
                           , wdZones ∷ [Zone] } deriving (Show, Eq)
