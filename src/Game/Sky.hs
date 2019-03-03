module Game.Sky where

import Data.List
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import GLUtil.Textures
import qualified Graphics.UI.GLFW as GLFW

import Game.Map
import Game.Settings
import Game.Data
import Game.State
import Game.Elev

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

drawSky :: State -> [GL.TextureObject] -> IO ()
drawSky state texs = do
  let snew = expandGrid $ stateSkies state
      n    = stateSkyTempZ state
  resequence_ (map (drawSkyRow n texs) snew)
  glFlush

drawSkyRow :: Int -> [GL.TextureObject] -> ([(Sky, Int)], Int) -> IO ()
drawSkyRow n texs (a, b) = resequence_ (map (drawSkySpot n texs b) a)

drawSkySpot :: Int -> [GL.TextureObject] -> Int -> (Sky, Int) -> IO ()
drawSkySpot n texs y (s, x) = withTextures2D [(texs!!10)] $ drawSkyTile n texs x y s

drawSkyTile :: Int -> [GL.TextureObject] -> Int -> Int -> Sky -> IO ()
drawSkyTile n texs x y (Sky lt mt ht ls hs) = do
  glLoadIdentity
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  case n of 1     -> case (lt) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+2)/18.0)-1) (1.0 - (abs (((t+2)-18.0)/18.0))) (1.0-((t+2)/18.0))
            2000  -> case (mt) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+2)/8.0)-1) (1.0 - (abs (((t+2)-8.0)/8.0))) (1.0-((t+2)/8.0))
            8000  -> case (ht) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+50)/8.0)-1) (1.0 - (abs (((t+50)-8.0)/8.0))) (1.0-((t+50)/8.0))
            16000 -> case (ht) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+60)/8.0)-1) (1.0 - (abs (((t+60)-8.0)/8.0))) (1.0-((t+60)/8.0))
            24000 -> case (ht) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+50)/8.0)-1) (1.0 - (abs (((t+50)-8.0)/8.0))) (1.0-((t+50)/8.0))
  drawSkySquare
  where thiszoom = fromIntegral theZoom

drawSkySquare :: IO ()
drawSkySquare = do
  glBegin GL_QUADS
  glTexCoord2f   0    1
  glVertex3f   (-1) (-1)  1
  glTexCoord2f   1    1
  glVertex3f     1  (-1)  1
  glTexCoord2f   1    0
  glVertex3f     1    1   1
  glTexCoord2f   0    0
  glVertex3f   (-1)   1   1
  glEnd

formatSkyTemp :: Int -> [Sky] -> (Int, Int) -> String
formatSkyTemp n ss (x, y) = getSkyTemp n s x y
  where s = (ss !! (x+gridw*y))

getSkyTemp :: Int -> Sky -> Int -> Int -> String
--getSkyTemp n     (Land t)             x y = "Solid Earth Temp: "       ++ (show t)
getSkyTemp 1     (Sky lt mt ht ls hs) x y = "Low Troposphere Temp: "   ++ (getSkyTempMaybe lt)
getSkyTemp 2000  (Sky lt mt ht ls hs) x y = "Mid Troposphere Temp: "   ++ (getSkyTempMaybe mt)
getSkyTemp 8000  (Sky lt mt ht ls hs) x y = "High Troposphere Temp: "  ++ (getSkyTempMaybe ht)
getSkyTemp 16000 (Sky lt mt ht ls hs) x y = "Low Stratosphere Temp: "  ++ (getSkyTempMaybe ls)
getSkyTemp 24000 (Sky lt mt ht ls hs) x y = "High Stratosphere Temp: " ++ (getSkyTempMaybe hs)

getSkyTempMaybe :: SkyZone -> String
getSkyTempMaybe s = case (s) of SkyZone t _ _ _ _ _ -> showFloatFoReal $ roundTo precision t
                                Land    t           -> "Land Temp: " ++ (showFloatFoReal $ roundTo precision t)

theExpanseAbove :: [Ocean] -> [Int] -> [Int] -> [Float] -> [Sky]
theExpanseAbove ocean grid elev light = newSkies ocean grid elev light

newSkies :: [Ocean] -> [Int] -> [Int] -> [Float] -> [Sky]
newSkies o g e l = zipWith4 newSky o g ne l
  where ne = normalizeElevs e

newSky :: Ocean -> Int -> Int -> Float -> Sky
newSky o g e l
 | (e > 2000) && (e < 8000)   = Sky { lowtroposphere   = Land 1.0
                                    , midtroposphere   = newZone o g e l 2000  0 0 0
                                    , hightroposphere  = newZone o g e l 8000  0 0 0
                                    , lowstratosphere  = newZone o g e l 16000 0 0 0
                                    , highstratosphere = newZone o g e l 24000 0 0 0
                                    }
 | (e > 8000) && (e < 16000)  = Sky { lowtroposphere   = Land 1.0
                                    , midtroposphere   = Land 1.0
                                    , hightroposphere  = newZone o g e l 8000  0 0 0
                                    , lowstratosphere  = newZone o g e l 16000 0 0 0
                                    , highstratosphere = newZone o g e l 24000 0 0 0
                                    }
 | (e > 16000) && (e < 24000) = Sky { lowtroposphere   = Land 1.0
                                    , midtroposphere   = Land 1.0
                                    , hightroposphere  = Land 1.0
                                    , lowstratosphere  = newZone o g e l 16000 0 0 0
                                    , highstratosphere = newZone o g e l 24000 0 0 0
                                    }
 | (e > 24000)                = Sky { lowtroposphere   = Land 1.0
                                    , midtroposphere   = Land 1.0
                                    , hightroposphere  = Land 1.0
                                    , lowstratosphere  = Land 1.0
                                    , highstratosphere = newZone o g e l 24000 0 0 0
                                    }
 | otherwise                  = Sky { lowtroposphere   = newZone o g e l 1     0 0 0
                                    , midtroposphere   = newZone o g e l 2000  0 0 0
                                    , hightroposphere  = newZone o g e l 8000  0 0 0
                                    , lowstratosphere  = newZone o g e l 16000 0 0 0
                                    , highstratosphere = newZone o g e l 24000 0 0 0
                                    }

newZone :: Ocean -> Int -> Int -> Float -> Int -> Float -> Float -> Float -> SkyZone
newZone o g e l n newvx newvy newvz = SkyZone { stemp = initSkyTemp o n
                                              , bar   = initSkyPres n
                                              , hum   = initSkyHum o n
                                              , svx   = newvx
                                              , svy   = newvy
                                              , svz   = newvz
                                              }

initSkyTemp :: Ocean -> Int -> Float
initSkyTemp (Dry _) 1     = 16.0
initSkyTemp s       1     = (16.0 + oceantemp) / 2.0
  where oceantemp = temp (epipelagic s)
initSkyTemp s       2000  = 6.0
initSkyTemp s       8000  = -50.0
initSkyTemp s       16000 = -60.0
initSkyTemp s       24000 = -50.0
initSkyTemp (Dry _) n     = 1.0
initSkyTemp s       n     = (temp (epipelagic s))

initSkyPres :: Int -> Float
initSkyPres n = -0.04*(fromIntegral n) + 1000.0

initSkyHum :: Ocean -> Int -> Float
initSkyHum (Dry _) 1     = 0.6
initSkyHum s       1     = 1.0
initSkyHum s       2000  = 0.8
initSkyHum s       8000  = 0.4
initSkyHum s       16000 = 0.7
initSkyHum s       24000 = 0.1
initSkyHum s       n     = 0.0

increaseSkyZ :: Int -> Int
increaseSkyZ 1     = 1
increaseSkyZ 2000  = 1
increaseSkyZ 8000  = 2000
increaseSkyZ 16000 = 8000
increaseSkyZ 24000 = 16000
increaseSkyZ x     = x

decreaseSkyZ :: Int -> Int
decreaseSkyZ 1     = 2000
decreaseSkyZ 2000  = 8000
decreaseSkyZ 8000  = 16000
decreaseSkyZ 16000 = 24000
decreaseSkyZ 24000 = 24000
decreaseSkyZ x     = x

tempSky :: [Sky] -> [Ocean] -> [Float] -> [Float] -> [Sky]
tempSky s o l ml = do
  let (sn, ss, se, sw) = cardinals s
  let xys           = yList
  map eqSkyTemp (zip7 s sn ss se sw (zip3 o l ml) xys)

eqSkyTemp :: (Sky, Sky, Sky, Sky, Sky, (Ocean, Float, Float), Int) -> Sky
eqSkyTemp ((Sky lt mt ht ls hs), sn, ss, se, sw, (o, l, ml), y) = Sky { lowtroposphere   = eqAirTemp 1     l ml lat lt (Land 1) mt        o sn ss se sw
                                                                      , midtroposphere   = eqAirTemp 2000  l ml lat mt lt       ht        o sn ss se sw
                                                                      , hightroposphere  = eqAirTemp 8000  l ml lat ht mt       ls        o sn ss se sw
                                                                      , lowstratosphere  = eqAirTemp 16000 l ml lat ls ht       hs        o sn ss se sw
                                                                      , highstratosphere = eqAirTemp 24000 l ml lat hs ls       (Space 1) o sn ss se sw
                                                                      }
  where lat = abs (fromIntegral (y) - (fromIntegral (gridh) / 2))

eqAirTemp :: Int -> Float -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> Ocean -> Sky -> Sky -> Sky -> Sky -> SkyZone
eqAirTemp n l ml lat z za zb o sn ss se sw = case (eqAirMaybe n l ml lat z za zb o sn ss se sw) of
                                               Nothing -> Land terratemp
                                               Just z  -> z

eqAirMaybe :: Int -> Float -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> Ocean -> Sky -> Sky -> Sky -> Sky -> Maybe SkyZone
eqAirMaybe n l ml lat z za zb o sn ss se sw = case (z) of
                                                Land t -> Just ( tempLand n t lat z za zb zn zs ze zw)
                                                SkyZone t p h svx svy svz -> Just ( newSkyZone n t p h svx svy svz z za zb zn zs ze zw lat l)
  where zn = getSkyZone n sn
        zs = getSkyZone n ss
        ze = getSkyZone n se
        zw = getSkyZone n sw

newSkyZone :: Int -> Float -> Float -> Float -> Float -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> Float -> Float -> SkyZone
newSkyZone n t p h svx0 svy0 svz0 z za zb zn zs ze zw lat l = SkyZone { stemp = newt
                                                            , bar   = newp
                                                            , hum   = newh
                                                            , svx   = svx
                                                            , svy   = svy
                                                            , svz   = svz
                                                            }
  where (svx, svy, svz) = calcWindV n l lat z za zb zn zs ze zw
        newt            = (pvnrtSky n p t norml z za zb zn zs ze zw)
        newp            = (pSkyZone n p z za zb zn zs ze zw)
        newh            = (humSkyZone n h z za zb zn zs ze zw)
        norml           = calcLight l lat

getSkyZone :: Int -> Sky -> SkyZone
getSkyZone 1     (Sky lt  _  _  _  _) = lt
getSkyZone 2000  (Sky  _ mt  _  _  _) = mt
getSkyZone 8000  (Sky  _  _ ht  _  _) = ht
getSkyZone 16000 (Sky  _  _  _ ls  _) = ls
getSkyZone 24000 (Sky  _  _  _  _ hs) = hs

tempLand :: Int -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone
tempLand n t0 lat z za zb zn zs ze zw = Land ((specificheatofterra*(t0+tterra) + tn + ts + te + tw)/(2*specificheatofterra+4.0))
  where tterra = terratemp / (1.0+(lat/((fromIntegral((n*gridh)))/180)))
        ta     = case ((getSkyT t0 za)<t0) of
                   True  -> (getSkyT t0 za)
                   False -> t0
        tb     = case ((getSkyT t0 zb)>t0) of
                   True  -> (getSkyT t0 zb)
                   False -> t0
        tn     = getSkyT t0 zn
        ts     = getSkyT t0 zs
        te     = getSkyT t0 ze
        tw     = getSkyT t0 zw

getSkyT :: Float -> SkyZone -> Float
getSkyT t0 (Land t)              = t
getSkyT t0 (SkyZone t _ _ _ _ _) = t

getSkyP :: Float -> SkyZone -> Float
getSkyP p0 (Land t)              = 1.01*p0
getSkyP p0 (SkyZone _ p _ _ _ _) = p

calcWindV :: Int -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> (Float, Float, Float)
calcWindV n l lat z za zb zn zs ze zw = (nvx, nvy, nvz)
  where nvx = min nn (max (((pw-p0)-(pe-p0))-(coriolispower*(cos (2*pi*(lat/((fromIntegral gridh))))))) (-nn))
        nvy = min nn (max ((ps - p0) - (pn - p0)) (-nn))
        nvz = min nn (max ((pa - p0) - (pb - p0)) (-nn))
        p0  = getSkyP 0.0 z
        pa  = (getSkyP p0 za) - (fromIntegral(decreaseSkyZ(n))*0.04) + (fromIntegral(n)*0.04)
        pb  = (getSkyP p0 zb) - (fromIntegral(increaseSkyZ(n))*0.04) + (fromIntegral(n)*0.04)
        pn  = getSkyP p0 zn
        ps  = getSkyP p0 zs
        pe  = getSkyP p0 ze
        pw  = getSkyP p0 zw
        nn  = 1.0+fromIntegral(n)*0.04

calcLight :: Float -> Float -> Float
calcLight l lat = l*(0.2+(cos (1.5*pi*(lat/((fromIntegral gridh))))))

pvnrtSky :: Int -> Float -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> Float
pvnrtSky n p t l z za zb zn zs ze zw = t

pSkyZone :: Int -> Float -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> Float
pSkyZone n p z za zb zn zs ze zw = p

humSkyZone :: Int -> Float -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> Float
humSkyZone n h z za zb zn zs ze zw = h
