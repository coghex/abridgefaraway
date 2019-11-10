module ABFA.Data where
-- datatypes are defined for various things

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString.Lazy.Char8 as BS

-- starts stops and pauses the timers
data TimerState = TStart | TStop | TPause | TNULL

-- some directions to make certain code easier to read
data Direction = DLeft | DRight | DUp | DDown | DNULL

-- the various types of possible biomes
data Biome      = BSea | BShallows | BDeeps | BValley | BCrags | BPlains | BFields | BWastes | BSteeps | BPeaks | BNULL deriving (Eq, Show)

-- settings datatype
data Settings = Settings
  { settingScreenW    :: Int
  , settingScreenH    :: Int
  , settingKeyLayout  :: KeyLayout
  , settingRefSW      :: Int
  , settingRefSH      :: Int
  , settingFullscreen :: Bool
  , settingFontSize   :: Int
  , settingFPS        :: Double
  , settingTimeSpeed  :: Int
  , settingAnimSpeed  :: Int
  , settingHistory    :: Int
  , settingPrecision  :: Int
  , settingGridW      :: Int
  , settingGridH      :: Int
  , settingZoneW      :: Int
  , settingZoneH      :: Int
  , settingWGSettings :: WorldGenSettings
  } deriving (Eq, Show)

-- world settings for the generator, these get manipulated into the world parameters
data WorldGenSettings = WorldGenSettings
  { wgCurrMap   :: Int
  , wgFudge     :: Int
  , wgSalt      :: Int
  , wgSugar     :: Float
  , wgVigor     :: Int
  , wgMinNConts :: Int
  , wgMaxNConts :: Int
  , wgMinSize   :: Int
  , wgMaxSize   :: Int
  , wgMinNSpots :: Int
  , wgMaxNSpots :: Int
  , wgSealevel  :: Int
  , wgPeaklevel :: Int
  } deriving (Eq, Show)

-- the layout of the keyboard
data KeyLayout = KeyLayout
  { keyC   :: String
  , keyR   :: String
  , keySPC :: String
  , keyESC :: String
  , keyRET :: String
  , keyDEL :: String
  , keySh  :: String
  , keyLFT :: String
  , keyRGT :: String
  , keyUPP :: String
  , keyDWN :: String
  , keyCL  :: String
  , keyCR  :: String
  , keyCU  :: String
  , keyCD  :: String
  } deriving (Eq, Show)

-- worldparameters for the generator
data WorldParams = WorldParams { wpNConts :: Int
                               , wpConts  :: ![(Int, Int)]
                               , wpSeeds  :: ![[(Int, Int)]]
                               , wpRands  :: ![[(Int, Int)]]
                               , wpSizes  :: ![Int]
                               , wpTypes  :: ![Biome]
                               , wpRandI  :: !Int
                               , wpRRands :: ![Int]
                               } deriving (Show, Eq)

-- the data about each zone
data Zone = Zone { latlong   :: (Int, Int)
                 , zonechunk :: ZoneChunk
                 }

-- zone data: grid number (2 bytes), cont number (2 bytes), elev number (1 byte)
data ZoneChunk = ZoneChunk { gbs :: BS.ByteString
                           , cbs :: BS.ByteString
                           , ebs :: BS.ByteString
                           }

-- a null zone
nullzone = Zone { latlong   = (0,0)
                , zonechunk = nullzonechunk
                }
nullzonechunk = ZoneChunk { gbs = nullbs
                          , cbs = nullbs
                          , ebs = nullbs
                          }

-- a null byte string
nullbs = BS.empty

-- the sky has 5 zones
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

--data Zone = Zone { grid :: [Int]
--                 , cont :: [Int]
--                 , elev :: [Float]
--                 , zazz :: [Zazz]
--                 , zgrd :: [Int]
--                 , emax :: Float
--                 , emin :: Float
--                 , nois :: Int
--                 , mapx :: Int
--                 , mapy :: Int
--                 , camx :: Float
--                 , camy :: Float
--                 , camz :: Int
--                 , curx :: Int
--                 , cury :: Int
--                 } deriving (Show, Eq)


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

