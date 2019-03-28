module ABFA.Settings where

import qualified Foreign.Lua as Lua

--these defaults will help if there is error
dscreenw = 1024::Int
dscreenh = 768::Int

data Settings = Settings
  { screenw    :: !Int
  , screenh    :: !Int
  , fullscreen :: !Bool
  , fps        :: !Double
  } deriving (Eq, Show)

importSettings :: IO (Settings)
importSettings = Lua.run $ do
  Lua.openlibs
  Lua.dofile "mods/base/base.lua"
  (sw, sh) <- Lua.callFunc "getscreensize" dscreenw dscreenh
  fs       <- Lua.getglobal "fullscreen" *> Lua.peek (-1)
  fps      <- Lua.getglobal "fps"        *> Lua.peek (-1)
  return $ makeSettings (sw::Int) (sh::Int) (fs::Bool) (fps::Double)

makeSettings :: Int -> Int -> Bool -> Double -> Settings
makeSettings sw sh fs fps = Settings { screenw    = sw
                                     , screenh    = sh
                                     , fullscreen = fs
                                     , fps        = fps }
