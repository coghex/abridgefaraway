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
initZoneCont state x y = genZoneCont x y conts seeds rands nconts BS.empty
  where conts   = wpConts      wparams
        seeds   = wpSeeds      wparams
        rands   = wpRands      wparams
        nconts  = wpNConts     wparams
        wparams = stateWParams state


-- generates the continent list for a single zone
genZoneCont :: Int -> Int -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> BS.ByteString -> BS.ByteString
genZoneCont _ _ []     []     []     _ bs0 = bs0
genZoneCont _ _ _      _      _      0 bs0 = bs0
genZoneCont x y (l:ls) (k:ks) (j:js) i bs0 = genZoneCont x y ls ks js (i-1) zg
  where zg = seedZoneCont x y 0 i (fst l) (snd l) k j bs0

-- expands the zone a more manageable data type before doing work
seedZoneCont :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> BS.ByteString -> BS.ByteString
seedZoneCont _ _ _ _ _  _  []     []     zc = zc
seedZoneCont x y i c x0 y0 (k:ks) (j:js) zc = seedZoneCont x y (i+1) c x0 y0 ks js zr
  where zr     = BS.empty
        zclist = bsToList zc

