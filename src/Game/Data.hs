module Game.Data where

import qualified Graphics.Rendering.OpenGL as GL

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
data Ocean = Dry Float | Sea { epipelagic    :: OceanZone
                             , mesopelagic   :: OceanZone
                             , bathypelagic  :: OceanZone
                             , abyssopelagic :: OceanZone
                             , hadopelagic   :: OceanZone
                             } deriving (Show, Eq)
data OceanZone = Solid Float | Ice Float | OceanZone { temp :: Float
                                                     , pres :: Float
                                                     , sal  :: Float
                                                     , vx   :: Float
                                                     , vy   :: Float
                                                     , vz   :: Float
                                                     } deriving (Show, Eq)

data Zone = Zone { grid :: [Int]
                 , cont :: [Int]
                 , elev :: [Float]
                 , emax :: Float
                 , emin :: Float
                 , nois :: Int
                 , mapx :: Int
                 , mapy :: Int
                 , camx :: Float
                 , camy :: Float
                 , camz :: Int
                 , curx :: Int
                 , cury :: Int
                 } deriving (Show, Eq)

data Unit = Unit { unittexs :: [GL.TextureObject]
                 , unittype :: Int
                 , zone     :: (Int, Int)
                 , position :: (Float, Float)
                 } deriving (Show, Eq)
