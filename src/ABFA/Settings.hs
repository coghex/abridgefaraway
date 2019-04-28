module ABFA.Settings where

import qualified Foreign.Lua as Lua
import ABFA.Data

--these reference settings will help if there is error, and will define the size of the boxes
refscreenw = 640::Int
refscreenh = 480::Int

-- imports the key layout
importKeyLayout :: Lua.State -> String -> IO (KeyLayout)
importKeyLayout ls fn = Lua.runWith ls $ do
  Lua.openlibs
  Lua.dofile $ fn ++ "keylayout.lua"
  ckey   <- Lua.getglobal "ckey"   *> Lua.peek (-1)
  rkey   <- Lua.getglobal "rkey"   *> Lua.peek (-1)
  spckey <- Lua.getglobal "spckey" *> Lua.peek (-1)
  esckey <- Lua.getglobal "esckey" *> Lua.peek (-1)
  retkey <- Lua.getglobal "retkey" *> Lua.peek (-1)
  delkey <- Lua.getglobal "delkey" *> Lua.peek (-1)
  shkey  <- Lua.getglobal "shkey"  *> Lua.peek (-1)
  return $ makeKeyLayout ckey rkey spckey esckey retkey delkey shkey

-- imports other settings
importSettings :: Lua.State -> String -> IO (Settings)
importSettings ls fn = do
  layout    <- importKeyLayout ls fn
  Lua.runWith ls $ do
    Lua.openlibs
    Lua.dofile $ fn ++ "config.lua"
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

makeKeyLayout :: String -> String -> String -> String -> String -> String -> String -> KeyLayout
makeKeyLayout ckey rkey spckey esckey retkey delkey shkey =
  KeyLayout { keyC   = ckey
            , keyR   = rkey
            , keySPC = spckey
            , keyESC = esckey
            , keyRET = retkey
            , keyDEL = delkey
            , keySh  = shkey }

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
                                                                         
-- reimports settings
reimportSettings :: Lua.State -> String -> IO (Settings)
reimportSettings ls fn = do
  layout    <- importKeyLayout ls fn
  Lua.runWith ls $ do
    Lua.openlibs
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

-- changes settings accordingly when window is resized

resizeSettings :: Settings -> Int -> Int -> Settings
resizeSettings settings w h =
  Settings { settingScreenW    = w
           , settingScreenH    = h
           , settingKeyLayout  = settingKeyLayout settings
           , settingRefSW      = settingRefSW settings
           , settingRefSH      = settingRefSH settings
           , settingFullscreen = settingFullscreen settings
           , settingFontSize   = settingFontSize settings
           , settingFPS        = settingFPS settings
           , settingTimeSpeed  = settingTimeSpeed settings
           , settingAnimSpeed  = settingAnimSpeed settings
           , settingHistory    = settingHistory settings
           , settingPrecision  = settingPrecision settings
           , settingGridW      = settingGridW settings
           , settingGridH      = settingGridH settings
           , settingWGSettings = settingWGSettings settings }


