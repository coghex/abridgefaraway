{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module ABFA.Zone where
-- the zone screen and data is defined

import qualified Data.List            as L
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Base64 (encode, decode)
import Control.Parallel.Strategies (parMap, rpar)
import ABFA.Data
import ABFA.Game
import ABFA.State
import ABFA.Map

nulltile = 0x00

-- gives an empty zone
blankZoneGrid :: State -> Int -> Int -> BS.ByteString
blankZoneGrid state x y = do
  let settings = stateSettings state
      zonew    = settingZoneW  settings
      zoneh    = settingZoneH  settings
  BS.replicate (fromIntegral (zonew*zoneh)) nulltile

-- returns zonechunks from a bytestring
convertToZoneChunks :: [BS.ByteString] -> [ZoneChunk]
convertToZoneChunks bss = map convertToZoneChunk bss

convertToZoneChunk :: BS.ByteString -> ZoneChunk
convertToZoneChunk bs0 = ZoneChunk { bs = bs0 }

initAllZoneGrids :: State -> [BS.ByteString]
initAllZoneGrids state = []

initAllZoneConts :: State -> [BS.ByteString]
initAllZoneConts state = []

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
genZone state x y zc0 conts seeds rands nconts = ZoneChunk { bs = bsr }
  where bsr = initZoneGrid state zcs
        zcs = initZoneCont state zc0

initZoneGrid :: State -> BS.ByteString -> BS.ByteString
initZoneGrid state str = str

initZoneCont :: State -> BS.ByteString -> BS.ByteString
initZoneCont state str = str

