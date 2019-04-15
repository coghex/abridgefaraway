module ABFA.Settings where

import qualified Foreign.Lua as Lua
import ABFA.Data

--these reference settings will help if there is error, and will define the size of the boxes
refscreenw = 640::Int
refscreenh = 480::Int

-- imports the key layout
importKeyLayout :: IO (KeyLayout)
importKeyLayout = Lua.run $ do
  Lua.openlibs
  Lua.dofile "mods/config/keylayout.lua"
  ckey   <- Lua.getglobal "ckey"   *> Lua.peek (-1)
  rkey   <- Lua.getglobal "rkey"   *> Lua.peek (-1)
  esckey <- Lua.getglobal "esckey" *> Lua.peek (-1)
  return $ makeKeyLayout ckey rkey esckey

-- imports other settings
importSettings :: IO (Settings)
importSettings = do
  layout    <- importKeyLayout
  Lua.run $ do
    Lua.openlibs
    Lua.dofile "mods/config/config.lua"
    (sw, sh)  <- Lua.callFunc "getscreensize" refscreenw refscreenh
    fs        <- Lua.getglobal "fullscreen" *> Lua.peek (-1)
    fsize     <- Lua.getglobal "fontsize"   *> Lua.peek (-1)
    fps       <- Lua.getglobal "fps"        *> Lua.peek (-1)
    timespeed <- Lua.getglobal "timespeed"  *> Lua.peek (-1)
    animspeed <- Lua.getglobal "animspeed"  *> Lua.peek (-1)
    history   <- Lua.getglobal "history"    *> Lua.peek (-1)
    precision <- Lua.getglobal "precision"  *> Lua.peek (-1)
    gridw     <- Lua.getglobal "gridw"      *> Lua.peek (-1)
    gridh     <- Lua.getglobal "gridh"      *> Lua.peek (-1)
    fudge     <- Lua.getglobal "fudge"      *> Lua.peek (-1)
    salt      <- Lua.getglobal "salt"       *> Lua.peek (-1)
    sugar     <- Lua.getglobal "sugar"      *> Lua.peek (-1)
    vigor     <- Lua.getglobal "vigor"      *> Lua.peek (-1)
    minnconts <- Lua.getglobal "minnconts"  *> Lua.peek (-1)
    maxnconts <- Lua.getglobal "maxnconts"  *> Lua.peek (-1)
    minsize   <- Lua.getglobal "minsize"    *> Lua.peek (-1)
    maxsize   <- Lua.getglobal "maxsize"    *> Lua.peek (-1)
    minnspots <- Lua.getglobal "minnspots"  *> Lua.peek (-1)
    maxnspots <- Lua.getglobal "maxnspots"  *> Lua.peek (-1)
    sealevel  <- Lua.getglobal "sealevel"   *> Lua.peek (-1)
    peaklevel <- Lua.getglobal "peaklevel"  *> Lua.peek (-1)
    return $ makeSettings (sw::Int) (sh::Int) (fs::Bool) (fsize::Int) (fps::Double) (timespeed::Int) (animspeed::Int) (history::Int) (precision::Int) (gridw::Int) (gridh::Int) (fudge::Int) (salt::Int) (sugar::Float) (vigor::Int) (minnconts::Int) (maxnconts::Int) (minsize::Int) (maxsize::Int) (minnspots::Int) (maxnspots::Int) (sealevel::Int) (peaklevel::Int) layout

makeSettings :: Int -> Int -> Bool -> Int -> Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Float -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> KeyLayout -> Settings
makeSettings sw sh fs fsize fps ts as h p gw gh f salt sugar vigor minnc maxnc mins maxs minns maxns sl pl layout =
  Settings { settingScreenW    = sw
           , settingScreenH    = sh
           , settingKeyLayout  = layout
           , settingRefSW      = refscreenw
           , settingRefSH      = refscreenh
           , settingFullscreen = fs
           , settingFontSize   = fsize
           , settingFPS        = fps
           , settingTimeSpeed  = ts
           , settingAnimSpeed  = as
           , settingHistory    = h
           , settingPrecision  = p
           , settingGridW      = gw
           , settingGridH      = gh
           , settingWGSettings = makeWGSettings f salt sugar vigor minnc maxnc mins maxs minns maxns sl pl }

makeKeyLayout :: String -> String -> String -> KeyLayout
makeKeyLayout ckey rkey esckey = KeyLayout { keyC   = ckey
                                           , keyR   = rkey
                                           , keyESC = esckey }

makeWGSettings :: Int -> Int -> Float -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> WorldGenSettings
makeWGSettings f sal sug vig minnc maxnc mins maxs minns maxns sl pl = WorldGenSettings { wgCurrMap   = 0
                                                                                        , wgFudge     = f
                                                                                        , wgSalt      = sal
                                                                                        , wgSugar     = sug
                                                                                        , wgVigor     = vig
                                                                                        , wgMinNConts = minnc
                                                                                        , wgMaxNConts = maxnc
                                                                                        , wgMinSize   = mins
                                                                                        , wgMaxSize   = maxs
                                                                                        , wgMinNSpots = minns
                                                                                        , wgMaxNSpots = maxns
                                                                                        , wgSealevel  = sl
                                                                                        , wgPeaklevel = pl }
                                                                         

