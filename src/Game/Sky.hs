module Game.Sky where

import Data.List

import Game.Map
import Game.Settings
import Game.Data

--data Sky = Sky { lowtroposphere   :: SkyZone
--               , midtroposphere   :: SkyZone
--               , hightroposphere  :: SkyZone
--               , lowstratosphere  :: SkyZone
--               , highstratosphere :: SkyZone
--               } deriving (Show, Eq)
--
--data SkyZone = SkyZone { stemp :: Float
--                       , bar   :: Float
--                       , hum   :: Float
--                       , svx   :: Float
--                       , svy   :: Float
--                       , svz   :: Float
--                       } deriving (Show, Eq)

theExpanseAbove :: [Ocean] -> [Int] -> [Int] -> [Float] -> [Sky]
theExpanseAbove ocean grid elev light = newSkies ocean grid elev light

newSkies :: [Ocean] -> [Int] -> [Int] -> [Float] -> [Sky]
newSkies o g e l = zipWith4 newSky o g e l

newSky :: Ocean -> Int -> Int -> Float -> Sky
newSky o g e l = Sky { lowtroposphere   = newZone o g e l 1     0 0 0
                     , midtroposphere   = newZone o g e l 2000  0 0 0
                     , hightroposphere  = newZone o g e l 8000  0 0 0
                     , lowstratosphere  = newZone o g e l 16000 0 0 0
                     , highstratosphere = newZone o g e l 24000 0 0 0
                     }

newZone :: Ocean -> Int -> Int -> Float -> Int -> Float -> Float -> Float -> SkyZone
newZone o g e l n newvx newvy newvz = SkyZone { stemp = initSkyTemp o
                                              , bar   = initSkyPres
                                              , hum   = initSkyHum o
                                              , svx   = newvx
                                              , svy   = newvy
                                              , svz   = newvz
                                              }

initSkyTemp :: Ocean -> Float
initSkyTemp (Dry _) = 1.0
initSkyTemp s       = (temp (epipelagic s))

initSkyPres :: Float
initSkyPres = 1.0

initSkyHum :: Ocean -> Float
initSkyHum (Dry _) = 1.0
initSkyHum s       = 2.0
