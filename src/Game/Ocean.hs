module Game.Ocean where

data Ocean = Ocean { epipelagic    :: OceanZone
                   , mesopelagic   :: OceanZone
                   , bathypelagic  :: OceanZone
                   , abyssopelagic :: OceanZone
                   , hadopelagic   :: OceanZone
                   } deriving (Show, Eq)

data OceanZone = OceanZone { temp :: Int
                           , pres :: Int
                           , vx   :: Int
                           , vy   :: Int
                           , vz   :: Int
                           } deriving (Show, Eq)
