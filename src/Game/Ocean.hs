module Game.Ocean where

data Ocean = Dry Int | Sea { epipelagic    :: OceanZone
                           , mesopelagic   :: OceanZone
                           , bathypelagic  :: OceanZone
                           , abyssopelagic :: OceanZone
                           , hadopelagic   :: OceanZone
                           } deriving (Show, Eq)

data OceanZone = OceanZone { temp :: Int
                           , pres :: Int
                           , sal  :: Int
                           , vx   :: Int
                           , vy   :: Int
                           , vz   :: Int
                           } deriving (Show, Eq)

newOceans :: [Int] -> [Ocean]
newOceans g = map newOcean g

newOcean :: Int -> Ocean
newOcean 0 = Sea { epipelagic    = newZone 0 0 0 0 0 0
                 , mesopelagic   = newZone 0 0 0 0 0 0
                 , bathypelagic  = newZone 0 0 0 0 0 0
                 , abyssopelagic = newZone 0 0 0 0 0 0
                 , hadopelagic   = newZone 0 0 0 0 0 0
                 }
newOcean g = Dry g

newZone :: Int -> Int -> Int -> Int -> Int -> Int -> OceanZone
newZone temp pres sal vx vy vz = OceanZone { temp = temp
                                           , pres = pres
                                           , sal  = sal
                                           , vx   = vx
                                           , vy   = vy
                                           , vz   = vz
                                           }



