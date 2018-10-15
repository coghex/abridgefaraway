module Game.Ocean where


data Ocean = Dry Int | Sea { epipelagic    :: OceanZone
                           , mesopelagic   :: OceanZone
                           , bathypelagic  :: OceanZone
                           , abyssopelagic :: OceanZone
                           , hadopelagic   :: OceanZone
                           } deriving (Show, Eq)

data OceanZone = Solid | OceanZone { temp :: Float
                                   , pres :: Float
                                   , sal  :: Float
                                   , vx   :: Float
                                   , vy   :: Float
                                   , vz   :: Float
                                   } deriving (Show, Eq)
 
theGreatSeas :: [Int] -> [Int] -> [Ocean]
theGreatSeas grid elev = newOceans grid elev

newOceans :: [Int] -> [Int] -> [Ocean]
newOceans g e = zipWith newOcean g e

newOcean :: Int -> Int -> Ocean
newOcean 0 e = Sea { epipelagic    = newZone e 0    0 0 0 0 0
                   , mesopelagic   = newZone e 200  0 0 0 0 0
                   , bathypelagic  = newZone e 1000 0 0 0 0 0
                   , abyssopelagic = newZone e 4000 0 0 0 0 0
                   , hadopelagic   = newZone e 6000 0 0 0 0 0
                   }
newOcean g e = Dry g

newZone :: Int -> Int -> Float -> Float -> Float -> Float -> Float -> OceanZone
newZone e n temp sal vx vy vz
  | (e < n)     = Solid
  | otherwise   = newOceanZone e


newOceanZone :: Int -> OceanZone
newOceanZone e = OceanZone { temp = 0
                         , pres = calcSeaPres e
                         , sal  = 0
                         , vx   = 0
                         , vy   = 0
                         , vz   = 0
                         }

calcSeaPres :: Int -> Float
calcSeaPres depth = fromIntegral(depth)/10.0
