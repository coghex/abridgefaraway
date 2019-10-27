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
generateZone :: State -> Int -> Int -> Zone
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

genZone :: State -> Int -> Int -> BS.ByteString -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> Zone
genZone state x y zc0 conts seeds rands nconts = Zone { latlong   = (x, y)
                                                      , zonechunk = genZoneChunk state x y zc0 conts seeds rands nconts
                                                      }

genZoneChunk :: State -> Int -> Int -> BS.ByteString -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> ZoneChunk
genZoneChunk state x y zc0 conts seeds rands nconts = ZoneChunk { gbs = zgs
                                                                , cbs = zcs
                                                                , ebs = BS.empty
                                                                }
  where zgs = initZoneGrid state zcs
        zcs = initZoneCont state x y

initZoneGrid :: State -> BS.ByteString -> BS.ByteString
initZoneGrid state str = listToBS zonelist 1
  where zonelist = take (zonew*zoneh) (repeat 0)
        zonew    = settingZoneW     settings
        zoneh    = settingZoneH     settings
        settings = stateSettings    state

initZoneCont :: State -> Int -> Int -> BS.ByteString
initZoneCont state x y = listToBS (genZoneCont gridw gridh zonew zoneh x y zc0 types sizes rrands conts seeds rands nconts) 1
  where conts    = wpConts           wparams
        seeds    = wpSeeds           wparams
        rands    = wpRands           wparams
        rrands   = wpRRands          wparams
        nconts   = wpNConts          wparams
        types    = wpTypes           wparams
        sizes    = wpSizes           wparams
        worldgen = settingWGSettings settings
        wparams  = stateWParams      state
        settings = stateSettings     state
        zonew    = settingZoneW      settings
        zoneh    = settingZoneH      settings
        gridw    = settingGridW      settings
        gridh    = settingGridH      settings
        zc0      = take (zonew*zoneh) (repeat 1)

-- generates the continent list for a single zone
genZoneCont :: Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Biome] -> [Int] -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
genZoneCont _     _     _     _     _ _ zg0 _     _     _      []     []     []     _ = zg0
genZoneCont _     _     _     _     _ _ zg0 _     _     _      _      _      _      0 = zg0
genZoneCont gridw gridh zonew zoneh x y zg0 types sizes rrands (l:ls) (k:ks) (j:js) n = do
  let zg = genZoneContChunk gridw gridh zonew zoneh x y 0 n (fst l) (snd l) zg0 types sizes rrands k j
  genZoneCont gridw gridh zonew zoneh x y zg types sizes rrands ls ks js (n-1)

-- generates a single continent
genZoneContChunk :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Biome] -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
genZoneContChunk _     _     _     _     _ _ _ _ _  _  zg _     _     _      []     []     = zg
genZoneContChunk gridw gridh zonew zoneh x y i c x0 y0 zg types sizes rrands (k:ks) (j:js) = do
  let nzg = expandZone zonew zoneh zg
      zg0 = map (genZoneContChunkRow gridw gridh zonew zoneh x y c i (fst k) (snd k) (fst j) (snd j) types sizes rrands) nzg
      zg1 = stripGrid zg0
      zg2 = flattenGrid zg1
  genZoneContChunk gridw gridh zonew zoneh x y (i+1) c x0 y0 zg2 types sizes rrands ks js

genZoneContChunkRow :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Biome] -> [Int] -> [Int] -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
genZoneContChunkRow gridw gridh zonew zoneh x y i c w x0 y0 z0 types sizes rrands (t1, t2) = (map (genZoneContChunkTile gridw gridh zonew zoneh x y i c t2 w x0 y0 z0 types sizes rrands) t1, t2)

-- this is basically the same as the worldcode
genZoneContChunkTile :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Biome] -> [Int] -> [Int] -> (Int, Int) -> (Int, Int)
genZoneContChunkTile gridw gridh zonew zoneh x y it c j w x0 y0 z0 types sizes rrands (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         y0         z0 t' < fromIntegral(8*(maxdist-cfudge))) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w (x0+gridw) y0         z0 t' < fromIntegral(8*(maxdist-cfudge))) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         (y0+gridh) z0 t' < fromIntegral(8*(maxdist-cfudge))) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w (x0-gridw) y0         z0 t' < fromIntegral(8*(maxdist-cfudge))) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         (y0-gridh) z0 t' < fromIntegral(8*(maxdist-cfudge))) = (t, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         y0         z0 t' <= fromIntegral((maxdist)*4))       = (r, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w (x0+gridw) y0         z0 t' <= fromIntegral((maxdist)*4))       = (r, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         (y0+gridh) z0 t' <= fromIntegral((maxdist)*4))       = (r, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w (x0-gridw) y0         z0 t' <= fromIntegral((maxdist)*4))       = (r, i)
  | (randstate == BValley) && (zoneDistance zonew zoneh x y i j w x0         (y0-gridh) z0 t' <= fromIntegral((maxdist)*4))       = (r, i)
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w x0         y0         z0 t' < fromIntegral((maxdist-cfudge)))   = (t, i)
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w (x0+gridw) y0         z0 t' < fromIntegral((maxdist-cfudge)))   = (t, i)
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w x0         (y0+gridh) z0 t' < fromIntegral((maxdist-cfudge)))   = (t, i)
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w (x0-gridw) y0         z0 t' < fromIntegral((maxdist-cfudge)))   = (t, i)
  | (randstate == BCrags)  && (zoneDistance zonew zoneh x y i j w x0         (y0-gridh) z0 t' < fromIntegral((maxdist-cfudge)))   = (t, i)
  |                            zoneDistance zonew zoneh x y i j w x0         y0         z0 t' <= fromIntegral(maxdist)            = (r, i)
  |                            zoneDistance zonew zoneh x y i j w (x0+gridw) y0         z0 t' <= fromIntegral(maxdist)            = (r, i)
  |                            zoneDistance zonew zoneh x y i j w x0         (y0+gridh) z0 t' <= fromIntegral(maxdist)            = (r, i)
  |                            zoneDistance zonew zoneh x y i j w (x0-gridw) y0         z0 t' <= fromIntegral(maxdist)            = (r, i)
  |                            zoneDistance zonew zoneh x y i j w x0         (y0-gridh) z0 t' <= fromIntegral(maxdist)            = (r, i)
  | otherwise                                                                                                       = (t, i)
  where
    randstate = types !! c
    r         = biomeToInt randstate
    maxdist   = 1000 * ((sizes) !! c)
    cfudge    = 2      * ((rrands !! c) - 1)
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

