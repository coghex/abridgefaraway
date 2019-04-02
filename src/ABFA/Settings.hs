module ABFA.Settings where

import qualified Foreign.Lua as Lua

--these reference settings will help if there is error, and will define the size of the boxes
refscreenw = 640::Int
refscreenh = 480::Int

data Settings = Settings
  { screenw    :: !Int
  , screenh    :: !Int
  , refsw      :: !Int
  , refsh      :: !Int
  , fullscreen :: !Bool
  , fontsize   :: !Int
  , fps        :: !Double
  } deriving (Eq, Show)

importSettings :: IO (Settings)
importSettings = Lua.run $ do
  Lua.openlibs
  Lua.dofile "mods/base/base.lua"
  (sw, sh) <- Lua.callFunc "getscreensize" refscreenw refscreenh
  fs       <- Lua.getglobal "fullscreen" *> Lua.peek (-1)
  fsize    <- Lua.getglobal "fontsize"   *> Lua.peek (-1)
  fps      <- Lua.getglobal "fps"        *> Lua.peek (-1)
  return $ makeSettings (sw::Int) (sh::Int) (fs::Bool) (fsize::Int) (fps::Double)

makeSettings :: Int -> Int -> Bool -> Int -> Double -> Settings
makeSettings sw sh fs fsize fps = Settings { screenw    = sw
                                     , screenh    = sh
                                     , refsw      = refscreenw
                                     , refsh      = refscreenh
                                     , fullscreen = fs
                                     , fontsize   = fsize
                                     , fps        = fps }
