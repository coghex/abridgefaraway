module ABFA.Data where
-- datatypes are defined for various things

import qualified Graphics.Rendering.OpenGL as GL


data WorldParams = NULLPARAMS | WorldParams { nconts :: Int
                                            , conts  :: ![(Int, Int)]
                                            , seeds  :: ![[(Int, Int)]]
                                            , rands  :: ![[(Int, Int)]]
                                            , sizes  :: ![Int]
                                            , types  :: ![Int]
                                            , randi  :: !Int
                                            , rrands :: ![Int]
                                            }

data Sky = Sky { lowtroposphere   :: SkyZone
               , midtroposphere   :: SkyZone
               , hightroposphere  :: SkyZone
               , lowstratosphere  :: SkyZone
               , highstratosphere :: SkyZone
               } deriving (Show, Eq)
data SkyZone = Land Float | Space Float | SkyZone { stemp :: Float
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
                 , zazz :: [Zazz]
                 , zgrd :: [Int]
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


data Zazz = Zazz { zazzx :: Int
                 , zazzy :: Int
                 , zazzs :: (Int, Int)
                 , zazzt :: Int
                 } deriving (Show, Eq)

data Action = NullAction
            | Idle { pos :: (Float, Float) }
            | Loaf { time :: Int }
            | MoveTo { dest  :: (Float, Float)
                     , speed :: Float
                     } deriving (Show, Eq)

data Unit = Unit { unittexs :: [GL.TextureObject]
                 , frame    :: Int
                 , unittype :: Int
                 , actions  :: [Action]
                 , zone     :: (Int, Int)
                 , position :: (Float, Float)
                 , dir      :: Int
                 } deriving (Show, Eq)

data Civ = Civ { civtype :: Int
               , units   :: [Unit]
               }

