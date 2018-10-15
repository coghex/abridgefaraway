module Game.Sky where


data Sky = Sky { surface :: SkyZone
               , low     :: SkyZone
               , mid     :: SkyZone
               , high    :: SkyZone
               , extreme :: SkyZone
               } deriving (Show, Eq)

data SkyZone = SkyZone { temp :: Int
                       , pres :: Int
                       , hum  :: Int
                       , vx   :: Int
                       , vy   :: Int
                       , vz   :: Int
                       } deriving (Show, Eq)

theExpanseAbove :: [Int] -> [Int] -> [Sky]
theExpanseAbove grid elev = newSkies elev

newSkies :: [Int] -> [Sky]
newSkies e = map newSky e

newSky :: Int -> Sky
newSky e = Sky { surface = newZone 0 0 0 0 0 0
               , low     = newZone 0 0 0 0 0 0
               , mid     = newZone 0 0 0 0 0 0
               , high    = newZone 0 0 0 0 0 0
               , extreme = newZone 0 0 0 0 0 0
               }

newZone :: Int -> Int -> Int -> Int -> Int -> Int -> SkyZone
newZone temp pres hum vx vy vz = SkyZone { temp = temp
                                         , pres = pres
                                         , hum  = hum
                                         , vx   = vx
                                         , vy   = vy
                                         , vz   = vz
                                         }
