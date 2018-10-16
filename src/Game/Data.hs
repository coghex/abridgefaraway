module Game.Data where

data Sky = Sky { lowtroposphere   :: SkyZone
               , midtroposphere   :: SkyZone
               , hightroposphere  :: SkyZone
               , lowstratosphere  :: SkyZone
               , highstratosphere :: SkyZone
               } deriving (Show, Eq)
data SkyZone = SkyZone { stemp :: Float
                       , bar   :: Float
                       , hum   :: Float
                       , svx   :: Float
                       , svy   :: Float
                       , svz   :: Float
                       } deriving (Show, Eq)
data Ocean = Dry Int | Sea { epipelagic    :: OceanZone
                           , mesopelagic   :: OceanZone
                           , bathypelagic  :: OceanZone
                           , abyssopelagic :: OceanZone
                           , hadopelagic   :: OceanZone
                           } deriving (Show, Eq)
data OceanZone = Solid Int | OceanZone { temp :: Float
                                   , pres :: Float
                                   , sal  :: Float
                                   , vx   :: Float
                                   , vy   :: Float
                                   , vz   :: Float
                                   } deriving (Show, Eq)

