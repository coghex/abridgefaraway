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
