{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module ABFA.Zone where
-- the zone screen and data is defined

import qualified Data.List            as L
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put
import Data.ByteString.Base64 (encode, decode)
import Control.Parallel.Strategies (parMap, rpar)
import ABFA.Data
import ABFA.Game
import ABFA.State
import ABFA.Map

nulltile = 0x00

-- gives an empty zone
blankZoneGrid :: State -> Int -> Int -> BS.ByteString
blankZoneGrid state x y = BS.replicate (fromIntegral (zonew*zoneh)) nulltile
  where settings = stateSettings state
        zonew    = settingZoneW  settings
        zoneh    = settingZoneH  settings

-- generates the zone at a xy pos
generateZone :: State -> Int -> Int -> ZoneChunk 
generateZone state x y = genZone state x y zc0 conts seeds rands nconts
  where zc0      = blankZoneGrid state x y
        wparams  = stateWParams  state
        conts    = wpConts       wparams
        seeds    = wpSeeds       wparams
        rands    = wpRands       wparams
        nconts   = wpNConts      wparams
        zonew    = settingZoneW  settings
        zoneh    = settingZoneH  settings
        settings = stateSettings state

genZone :: State -> Int -> Int -> BS.ByteString -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> ZoneChunk
genZone state x y zc0 conts seeds rands nconts = ZoneChunk { gbs = zgs
                                                           , cbs = zcs
                                                           , pbs = BS.empty
                                                           }
  where zgs = initZoneGrid state zcs
        zcs = initZoneCont state x y

initZoneGrid :: State -> BS.ByteString -> BS.ByteString
initZoneGrid state str = str

initZoneCont :: State -> Int -> Int -> BS.ByteString
initZoneCont state x y = genZoneCont gridw gridh zonew zoneh types sizes rrands minncs x y conts seeds rands nconts BS.empty
  where conts    = wpConts           wparams
        seeds    = wpSeeds           wparams
        rands    = wpRands           wparams
        nconts   = wpNConts          wparams
        types    = wpTypes           wparams
        sizes    = wpSizes           wparams
        rrands   = wpRRands          wparams
        minncs   = wgMinNConts       worldgen
        worldgen = settingWGSettings settings
        wparams  = stateWParams      state
        settings = stateSettings     state
        zonew    = settingZoneW      settings
        zoneh    = settingZoneH      settings
        gridw    = settingGridW      settings
        gridh    = settingGridH      settings

-- generates the continent list for a single zone
genZoneCont :: Int -> Int -> Int -> Int -> [Biome] -> [Int] -> [Int] -> Int -> Int -> Int -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> BS.ByteString -> BS.ByteString
genZoneCont _     _     _     _     _     _     _      _      _ _ []     []     []     _ bs0 = bs0
genZoneCont _     _     _     _     _     _     _      _      _ _ _      _      _      0 bs0 = bs0
genZoneCont gridw gridh zonew zoneh types sizes rrands minncs x y (l:ls) (k:ks) (j:js) i bs0 = genZoneCont gridw gridh zonew zoneh types sizes rrands minncs x y ls ks js (i-1) zg
  where zg = seedZoneCont gridw gridh zonew zoneh types sizes rrands minncs x y 0 i (fst l) (snd l) k j bs0

-- expands the zone a more manageable data type before doing work
seedZoneCont :: Int -> Int -> Int -> Int -> [Biome] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> BS.ByteString -> BS.ByteString
seedZoneCont _     _     _     _     _     _     _      _      _ _ _ _ _  _  []     []     zc = zc
seedZoneCont gridw gridh zonew zoneh types sizes rrands minncs x y i c x0 y0 (k:ks) (j:js) zc = seedZoneCont gridw gridh zonew zoneh types sizes rrands minncs x y (i+1) c x0 y0 ks js zr
  where zr     = listToBS zg2 1
        zclist = bsToList zc  1
        nzg    = expandZone zonew zoneh zclist
        zg0    = parMap rpar (seedZoneContRow gridw gridh zonew zoneh types sizes rrands minncs x y i c (fst k) (snd k) (fst j) (snd j)) nzg
        zg1    = stripGrid zg0
        zg2    = flattenGrid zg1

-- seeds each row individually
seedZoneContRow :: Int -> Int -> Int -> Int -> [Biome] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedZoneContRow gridw gridh zonew zoneh types sizes rrands minncs x y i c w x0 y0 z0 (t1, t2) = (map (seedZoneContTile gridw gridh zonew zoneh types sizes rrands minncs x y i c t2 w x0 y0 z0) t1, t2)

-- seeds each tile
seedZoneContTile :: Int -> Int -> Int -> Int -> [Biome] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedZoneContTile gridw gridh zonew zoneh types sizes rrands minncs x y it c j w x0 y0 z0 (t, i)
  | ((randstate == BShallows) || (randstate == BDeeps) || (randstate == BPeaks))                        = (t, i)
  | (randstate == BSea)    && (zoneDistance zonew zoneh x y i j w x0         y0         z0 t' <= maxdist)           = (r, i)
  | (randstate == BSea)    && (zoneDistance zonew zoneh x y i j w (x0+gridw) y0         z0 t' <= maxdist)           = (r, i)
  | (randstate == BSea)    && (zoneDistance zonew zoneh x y i j w x0         (y0+gridh) z0 t' <= maxdist)           = (r, i)
  | (randstate == BSea)    && (zoneDistance zonew zoneh x y i j w (x0-gridw) y0         z0 t' <= maxdist)           = (r, i)
  | (randstate == BSea)    && (zoneDistance zonew zoneh x y i j w x0         (y0-gridh) z0 t' <= maxdist)           = (r, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         y0         z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w (x0+gridw) y0         z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         (y0+gridh) z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w (x0-gridw) y0         z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         (y0-gridh) z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         y0         z0 t' <= (4*maxdist))       = (r, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w (x0+gridw) y0         z0 t' <= (4*maxdist))       = (r, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         (y0+gridh) z0 t' <= (4*maxdist))       = (r, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w (x0-gridw) y0         z0 t' <= (4*maxdist))       = (r, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         (y0-gridh) z0 t' <= (4*maxdist))       = (r, i) 
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w x0         y0         z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w (x0+gridw) y0         z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w x0         (y0+gridh) z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w (x0-gridw) y0         z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w x0         (y0-gridh) z0 t' < maxdist-cfudge)     = (t, i)
  | otherwise                                                                                           = (t, i)
  where
    randstate = types !! c
    r         = biomeToInt randstate
    maxdist   = 1000.0 * fromIntegral(((sizes) !! c))
    cfudge    = 2.0 * fromIntegral(((rrands) !! (c)) - minncs)
    t'        = fromIntegral t

-- a simple conversion function to abstract biome numbers
intToBiome :: Int -> Biome
intToBiome 1  = BSea
intToBiome 2  = BShallows
intToBiome 3  = BDeeps
intToBiome 4  = BValley
intToBiome 5  = BCrags
intToBiome 6  = BPlains
intToBiome 7  = BFields
intToBiome 8  = BWastes
intToBiome 9  = BSteeps
intToBiome 10 = BPeaks
intToBiome _  = BNULL

biomeToInt :: Biome -> Int
biomeToInt BSea      = 1
biomeToInt BShallows = 2
biomeToInt BDeeps    = 3
biomeToInt BValley   = 4
biomeToInt BCrags    = 5
biomeToInt BPlains   = 6
biomeToInt BFields   = 7
biomeToInt BWastes   = 8
biomeToInt BSteeps   = 9
biomeToInt BPeaks    = 10
biomeToInt _         = 0

