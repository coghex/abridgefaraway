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

drawRain :: State -> [GL.TextureObject] -> IO ()
drawRain state texs = do
  let snew = expandGrid $ stateSkies state
      n    = stateRainZ state
  resequence_ (map (drawRainRow n texs) snew)

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

drawRainRow :: Int -> [GL.TextureObject] -> ([(Sky, Int)], Int) -> IO ()
drawRainRow n texs (a, b) = resequence_ (map (drawRainSpot n texs b) a)

drawRainSpot :: Int -> [GL.TextureObject] -> Int -> (Sky, Int) -> IO ()
drawRainSpot n texs y (s, x) = withTextures2D [(texs!!10)] $ drawRainTile n texs x y s

drawRainTile :: Int -> [GL.TextureObject] -> Int -> Int -> Sky -> IO ()
drawRainTile n texs x y (Sky lt mt ht ls hs) = do
  glLoadIdentity
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  case n of 1     -> case (lt) of Land t              -> glColor3f 0.0 0.0 0.0
                                  SkyZone _ _ h _ _ _ -> glColor3f   h   h   h
            2000  -> case (mt) of Land t              -> glColor3f 0.0 0.0 0.0
                                  SkyZone _ _ h _ _ _ -> glColor3f   h   h   h
            8000  -> case (ht) of Land t              -> glColor3f 0.0 0.0 0.0
                                  SkyZone _ _ h _ _ _ -> glColor3f   h   h   h
            16000 -> case (ls) of Land t              -> glColor3f 0.0 0.0 0.0
                                  SkyZone _ _ h _ _ _ -> glColor3f   h   h   h
            24000 -> case (hs) of Land t              -> glColor3f 0.0 0.0 0.0
                                  SkyZone _ _ h _ _ _ -> glColor3f   h   h   h
  drawSkySquare
  where thiszoom = fromIntegral theZoom


drawNullTile :: Int -> [GL.TextureObject] -> [Int] -> [Int] -> Int -> Int -> IO ()
drawNullTile n texs g e x y = do
  glLoadIdentity
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  setWindColor n 1.0 (tapGrid g x y) ((tapGrid e x y)-(round sealevel))
  drawSkySquare
  where thiszoom = fromIntegral theZoom

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

drawWind :: State -> [GL.TextureObject] -> IO ()
drawWind state texs = do
  let snew = expandGrid $ stateSkies state
      n    = stateWindZ state
      g    = stateGrid state
      e    = stateElev state
  resequence_ ((map (drawWindBackgroundRow n texs g e) snew) ++ (map (drawWindRow n texs g e) snew))
  glFlush

drawWindRow :: Int -> [GL.TextureObject] -> [Int] -> [Int] -> ([(Sky, Int)], Int) -> IO ()
drawWindRow n texs g e (a, b) = resequence_ (map (drawWindSpot n texs g e b) a)

drawWindBackgroundRow :: Int -> [GL.TextureObject] -> [Int] -> [Int] -> ([(Sky, Int)], Int) -> IO ()
drawWindBackgroundRow n texs g e (a, b) = resequence_ (map (drawWindBackgroundSpot n texs g e b) a)

drawWindSpot :: Int -> [GL.TextureObject] -> [Int] -> [Int] -> Int -> (Sky, Int) -> IO ()
drawWindSpot 1     texs g e y ((Sky lt  _  _  _  _), x) = drawWindTile 1     texs g e x y lt
drawWindSpot 2000  texs g e y ((Sky  _ mt  _  _  _), x) = drawWindTile 2000  texs g e x y mt
drawWindSpot 8000  texs g e y ((Sky  _  _ ht  _  _), x) = drawWindTile 8000  texs g e x y ht
drawWindSpot 16000 texs g e y ((Sky  _  _  _ ls  _), x) = drawWindTile 16000 texs g e x y ls
drawWindSpot 24000 texs g e y ((Sky  _  _  _  _ hs), x) = drawWindTile 24900 texs g e x y hs

drawWindBackgroundSpot :: Int -> [GL.TextureObject] -> [Int] -> [Int] -> Int -> (Sky, Int) -> IO ()
drawWindBackgroundSpot 1     texs g e y ((Sky lt  _  _  _  _), x) = drawWindBackgroundTile 1     texs g e x y lt
drawWindBackgroundSpot 2000  texs g e y ((Sky  _ mt  _  _  _), x) = drawWindBackgroundTile 2000  texs g e x y mt
drawWindBackgroundSpot 8000  texs g e y ((Sky  _  _ ht  _  _), x) = drawWindBackgroundTile 8000  texs g e x y ht
drawWindBackgroundSpot 16000 texs g e y ((Sky  _  _  _ ls  _), x) = drawWindBackgroundTile 16000 texs g e x y ls
drawWindBackgroundSpot 24000 texs g e y ((Sky  _  _  _  _ hs), x) = drawWindBackgroundTile 24000 texs g e x y hs

drawWindBackgroundTile :: Int -> [GL.TextureObject] -> [Int] -> [Int] -> Int -> Int -> SkyZone -> IO ()
drawWindBackgroundTile n texs g e x y (Land _)               = withTextures2D [(texs!!10)] $ drawNullTile n texs g e x y
drawWindBackgroundTile n texs g e x y (SkyZone _ _ _ _ _ vz) = withTextures2D [(texs!!10)] $ drawVZTile n vz texs g e x y

drawVZTile :: Int -> Float -> [GL.TextureObject] -> [Int] -> [Int] -> Int -> Int -> IO ()
drawVZTile n vz texs g e x y = do
  glLoadIdentity
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  setWindColor n 1.0 (tapGrid g x y) ((tapGrid e x y)-(round sealevel))
  drawSkySquare
  where thiszoom = fromIntegral theZoom

setWindColor :: Int -> Float -> Int -> Int -> IO ()
setWindColor n     vz 1 _ = glColor3f vz       1.0 1.0
setWindColor n     vz 7 _ = glColor3f vz       1.0 1.0
setWindColor 1     vz g e
  | e < 1     = glColor3f vz       1.0 1.0
  | e < 2000  = glColor3f (0.5*vz) 0.5 0.5
  | e < 8000  = glColor3f (0.0*vz) 0.0 0.0
  | e < 16000 = glColor3f (0.0*vz) 0.0 0.0
  | e < 24000 = glColor3f (0.0*vz) 0.0 0.0
  | otherwise = glColor3f 0.0      0.0 0.0
setWindColor 2000  vz g e
  | e < 1     = glColor3f vz       1.0 1.0
  | e < 2000  = glColor3f (0.6*vz) 0.6 0.6
  | e < 8000  = glColor3f (0.2*vz) 0.2 0.2
  | e < 16000 = glColor3f (0.0*vz) 0.0 0.0
  | e < 24000 = glColor3f (0.0*vz) 0.0 0.0
  | otherwise = glColor3f 0.0      0.0 0.0
setWindColor 8000  vz g e
  | e < 1     = glColor3f vz       1.0 1.0
  | e < 2000  = glColor3f (0.8*vz) 0.8 0.8
  | e < 8000  = glColor3f (0.5*vz) 0.5 0.5
  | e < 16000 = glColor3f (0.2*vz) 0.2 0.2
  | e < 24000 = glColor3f (0.0*vz) 0.0 0.0
  | otherwise = glColor3f 0.0      0.0 0.0
setWindColor 16000 vz g e
  | e < 1     = glColor3f vz       1.0 1.0
  | e < 2000  = glColor3f (1.0*vz) 1.0 1.0
  | e < 8000  = glColor3f (0.8*vz) 0.8 0.8
  | e < 16000 = glColor3f (0.5*vz) 0.5 0.5
  | e < 24000 = glColor3f (0.2*vz) 0.2 0.2
  | otherwise = glColor3f 0.0      0.0 0.0
setWindColor 24000 vz g e
  | e < 1     = glColor3f vz       1.0 1.0
  | e < 2000  = glColor3f (1.0*vz) 1.0 1.0
  | e < 8000  = glColor3f (1.0*vz) 1.0 1.0
  | e < 16000 = glColor3f (0.8*vz) 0.8 0.8
  | e < 24000 = glColor3f (0.5*vz) 0.5 0.5
  | otherwise = glColor3f 0.2      0.2 0.2

drawWindTile :: Int -> [GL.TextureObject] -> [Int] -> [Int] -> Int -> Int -> SkyZone -> IO ()
drawWindTile n texs g e x y (Land _)                             = withTextures2D [(texs!!10)] $ drawNullTile n texs g e x y
drawWindTile n texs g e x y (SkyZone _ _ _ vx vy vz)
  | ((abs vx) < currentslevel) && (vy > currentslevel)       = withTextures2D [(texs!!26)] $ drawWindTileTex n vy                       vz g e texs x y
  | (vx > currentslevel)       && (vy > currentslevel)       = withTextures2D [(texs!!27)] $ drawWindTileTex n ((vx+vy)/(sqrt 2))       vz g e texs x y
  | (vx > (-currentslevel))    && (vy > currentslevel)       = withTextures2D [(texs!!25)] $ drawWindTileTex n (((-vx)+vy)/(sqrt 2))    vz g e texs x y
  | ((abs vx) < currentslevel) && (vy < (-currentslevel))    = withTextures2D [(texs!!22)] $ drawWindTileTex n (-vy)                    vz g e texs x y
  | (vx > currentslevel)       && (vy < (-currentslevel))    = withTextures2D [(texs!!28)] $ drawWindTileTex n ((vx+(-vy))/(sqrt 2))    vz g e texs x y
  | (vx > (-currentslevel))    && (vy < (-currentslevel))    = withTextures2D [(texs!!23)] $ drawWindTileTex n (((-vx)+(-vy))/(sqrt 2)) vz g e texs x y
  | (vx > currentslevel)       && ((abs vy) < currentslevel) = withTextures2D [(texs!!27)] $ drawWindTileTex n vx                       vz g e texs x y
  | (vx < (-currentslevel))    && ((abs vy) < currentslevel) = withTextures2D [(texs!!24)] $ drawWindTileTex n (-vy)                    vz g e texs x y
  | otherwise                                                = withTextures2D [(texs!!30)] $ drawVZTile n vz texs g e x y

drawWindTileTex :: Int -> Float -> Float -> [Int] -> [Int] -> [GL.TextureObject] -> Int -> Int -> IO ()
drawWindTileTex 1     size vz g e texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  setWindColor 1 1.0 (tapGrid g x y) ((tapGrid e x y)-(round sealevel))
  drawSkySquare
  GL.depthFunc GL.$= Nothing
  where currentszoom = min 1 (max (size*0.06) 0)
        thiszoom     = fromIntegral theZoom
drawWindTileTex 2000  size vz g e texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  setWindColor 2000 1.0 (tapGrid g x y) ((tapGrid e x y)-(round sealevel))
  drawSkySquare
  GL.depthFunc GL.$= Nothing
  where currentszoom = min 1 (max (size*0.01) 0)
        thiszoom     = fromIntegral theZoom
drawWindTileTex 8000  size vz g e texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  setWindColor 8000 1.0 (tapGrid g x y) ((tapGrid e x y)-(round sealevel))
  drawSkySquare
  GL.depthFunc GL.$= Nothing
  where currentszoom = min 1 (max (size*0.01) 0)
        thiszoom     = fromIntegral theZoom
drawWindTileTex n     size vz g e texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  setWindColor n 1.0 (tapGrid g x y) ((tapGrid e x y)-(round sealevel))
  drawSkySquare
  GL.depthFunc GL.$= Nothing
  where currentszoom = min 1 (max (size*0.01) 0)
        thiszoom     = fromIntegral theZoom

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
 | (e > 2000) && (e < 8000)   = Sky { lowtroposphere   = Land 20.0
                                    , midtroposphere   = newZone o g e l 2000  0 0 0
                                    , hightroposphere  = newZone o g e l 8000  0 0 0
                                    , lowstratosphere  = newZone o g e l 16000 0 0 0
                                    , highstratosphere = newZone o g e l 24000 0 0 0
                                    }
 | (e > 8000) && (e < 16000)  = Sky { lowtroposphere   = Land 20.0
                                    , midtroposphere   = Land 20.0
                                    , hightroposphere  = newZone o g e l 8000  0 0 0
                                    , lowstratosphere  = newZone o g e l 16000 0 0 0
                                    , highstratosphere = newZone o g e l 24000 0 0 0
                                    }
 | (e > 16000) && (e < 24000) = Sky { lowtroposphere   = Land 20.0
                                    , midtroposphere   = Land 20.0
                                    , hightroposphere  = Land 20.0
                                    , lowstratosphere  = newZone o g e l 16000 0 0 0
                                    , highstratosphere = newZone o g e l 24000 0 0 0
                                    }
 | (e > 24000)                = Sky { lowtroposphere   = Land 20.0
                                    , midtroposphere   = Land 20.0
                                    , hightroposphere  = Land 20.0
                                    , lowstratosphere  = Land 20.0
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

tempSky :: [Sky] -> [Ocean] -> [Int] -> [Float] -> [Float] -> [Sky]
tempSky s o e l ml = do
  let (sn, ss, se, sw) = cardinals s
  let xys           = yList
  map eqSkyTemp (zip7 s sn ss se sw (zip4 o e l ml) xys)

eqSkyTemp :: (Sky, Sky, Sky, Sky, Sky, (Ocean, Int, Float, Float), Int) -> Sky
eqSkyTemp ((Sky lt mt ht ls hs), sn, ss, se, sw, (o, e, l, ml), y) = Sky { lowtroposphere   = eqAirTemp 1     e l ml lat lt (Land 1) mt        o sn ss se sw
                                                                         , midtroposphere   = eqAirTemp 2000  e l ml lat mt lt       ht        o sn ss se sw
                                                                         , hightroposphere  = eqAirTemp 8000  e l ml lat ht mt       ls        o sn ss se sw
                                                                         , lowstratosphere  = eqAirTemp 16000 e l ml lat ls ht       hs        o sn ss se sw
                                                                         , highstratosphere = eqAirTemp 24000 e l ml lat hs ls       (Space 1) o sn ss se sw
                                                                         }
  where lat = abs (fromIntegral (y) - (fromIntegral (gridh) / 2))

eqAirTemp :: Int -> Int -> Float -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> Ocean -> Sky -> Sky -> Sky -> Sky -> SkyZone
eqAirTemp n e l ml lat z za zb o sn ss se sw = case (eqAirMaybe n e l ml lat z za zb o sn ss se sw) of
                                                 Nothing -> Land terratemp
                                                 Just z  -> z

eqAirMaybe :: Int -> Int -> Float -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> Ocean -> Sky -> Sky -> Sky -> Sky -> Maybe SkyZone
eqAirMaybe n e l ml lat z za zb o sn ss se sw = case (z) of
                                                  Land t -> Just ( tempLand n t lat z za zb zn zs ze zw)
                                                  SkyZone t p h svx svy svz -> Just ( newSkyZone n t p h o svx svy svz z za zb zn zs ze zw lat e l)
  where zn = getSkyZone n sn
        zs = getSkyZone n ss
        ze = getSkyZone n se
        zw = getSkyZone n sw

newSkyZone :: Int -> Float -> Float -> Float -> Ocean -> Float -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> Float -> Int -> Float -> SkyZone
newSkyZone n t p h o svx0 svy0 svz0 z za zb zn zs ze zw lat e l = SkyZone { stemp = newt
                                                                          , bar   = newp
                                                                          , hum   = newh
                                                                          , svx   = svx
                                                                          , svy   = svy
                                                                          , svz   = svz
                                                                          }
  where (svx, svy, svz) = calcWindV n l lat z za zb zn zs ze zw
        newt            = (pvnrtSky n p t e o norml z za zb zn zs ze zw)
        newp            = (pSkyZone n p z e za zb zn zs ze zw)
        newh            = (humSkyZone n lat t h o z za zb zn zs ze zw)
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

getWindV :: (Float, Float, Float) -> SkyZone -> (Float, Float, Float)
getWindV (vx0, vy0, vz0) (Land _)                 = (0.0, 0.0, 0.0)
getWindV (vx0, vy0, vz0) (SkyZone _ _ _ vx vy vz) = ( vx,  vy,  vz)

getSkyT :: Float -> SkyZone -> Float
getSkyT t0 (Land t)              = t
getSkyT t0 (SkyZone t _ _ _ _ _) = t

getSkyP :: Float -> SkyZone -> Float
getSkyP p0 (Land t)              = 1.01*p0
getSkyP p0 (SkyZone _ p _ _ _ _) = p

getSkyH :: Float -> SkyZone -> Float
getSkyH h0 (Land t)              = h0
getSkyH h0 (SkyZone _ _ h _ _ _) = h


calcWindV :: Int -> Float -> Float -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> (Float, Float, Float)
calcWindV n l lat z za zb zn zs ze zw = (nvx, nvy, nvz)
  where nvx = min nn (max (((pw-p0)-(pe-p0))-(100*coriolispower*(cos (2/3*pi*(lat/((fromIntegral gridh))))))) (-nn))
        nvy = min nn (max ((ps - p0) - (pn - p0)) (-nn))
        nvz = min nn (max ((pa - p0) - (pb - p0)) (-nn))
        p0  = getSkyP 0.0 z
        pa  = (getSkyP p0 za) - ((fromIntegral(decreaseSkyZ(n))*0.04) + 1000.0)
        pb  = (getSkyP p0 zb) - ((fromIntegral(increaseSkyZ(n))*0.04) + 1000.0)
        pn  = getSkyP p0 zn
        ps  = getSkyP p0 zs
        pe  = getSkyP p0 ze
        pw  = getSkyP p0 zw
        nn  = 100.0+fromIntegral(n)*0.04

calcLight :: Float -> Float -> Float
calcLight l lat = l*(0.2+(cos (1.5*pi*(lat/((fromIntegral gridh))))))

pvnrtSky :: Int -> Float -> Float -> Int -> Ocean -> Float -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> Float
pvnrtSky 1 p t e (Sea ep _ _ _ _) l z za zb zn zs ze zw = ((t*specificheatofair)+(newt)+(ot))/(specificheatofair+2)
  where ot = getOTemp ep
        h  = hum z
        newt = p*0.01 + ((l-1)*(1-h))
pvnrtSky 1 p t e (Dry te)         l z za zb zn zs ze zw = ((t*specificheatofair)+(newt)+(et))/(specificheatofair+1)
  where h  = hum z
        et = te
        newt = p*0.01 + (40*(l-1)*(1-h))
pvnrtSky n p t e _                l z za zb zn zs ze zw = ((t*specificheatofair)+(newt))/(specificheatofair+1)
  where h  = hum z
        newt = p*0.01 + (0.2*(l-1)*(1-h)) - 40

pSkyZone :: Int -> Float -> SkyZone -> Int -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> Float
pSkyZone n p z e za zb zn zs ze zw = ((p*momentumofair) + pbase + pn + ps + pe + pw)/(momentumofair+5)
  where pbase = 1000.0 - (0.04*(fromIntegral n))
        pn    = (1+((-vyn)/momentumofair))*(getSkyP p zn)
        ps    = (1+(vys/momentumofair))*(getSkyP p zs)
        pe    = (1+((-vxe)/momentumofair))*(getSkyP p ze)
        pw    = (1+(vxw/momentumofair))*(getSkyP p zw)
        ( vx,  vy,  vz) = getWindV (0.0, 0.0, 0.0) z
        (vxa, vya, vza) = getWindV ( vx,  vy,  vz) za
        (vxb, vyb, vzb) = getWindV ( vx,  vy,  vz) zb
        (vxn, vyn, vzn) = getWindV ( vx,  vy,  vz) zn
        (vxs, vys, vzs) = getWindV ( vx,  vy,  vz) zs
        (vxe, vye, vze) = getWindV ( vx,  vy,  vz) ze
        (vxw, vyw, vzw) = getWindV ( vx,  vy,  vz) zw

humSkyZone :: Int -> Float -> Float-> Float -> Ocean -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> SkyZone -> Float
humSkyZone n lat t h (Sea ep _ _ _ _) z za zb zn zs ze zw = ((h*specificheatofair)+newh+windh)/(specificheatofair+2)
  where maxh = min 1.0 (max 0.1 ((t) / 24.0))
        ot   = getOTemp ep
        newh = min (ot/24.0) maxh
        windh = max hummin (min 1.0 ((hn+hs+he+hw)/4.0))
        hummin = 0.5+(0.5*(cos (7*pi*(lat/((fromIntegral gridh))))))
        hn   = (1+((-vyn)/momentumofair))*(getSkyH h zn)
        hs   = (1+(vys/momentumofair))*(getSkyH h zs)
        he   = (1+((-vxe)/momentumofair))*(getSkyH h ze)
        hw   = (1+(vxw/momentumofair))*(getSkyH h zw)
        ( vx,  vy,  vz) = getWindV (0.0, 0.0, 0.0) z
        (vxa, vya, vza) = getWindV ( vx,  vy,  vz) za
        (vxb, vyb, vzb) = getWindV ( vx,  vy,  vz) zb
        (vxn, vyn, vzn) = getWindV ( vx,  vy,  vz) zn
        (vxs, vys, vzs) = getWindV ( vx,  vy,  vz) zs
        (vxe, vye, vze) = getWindV ( vx,  vy,  vz) ze
        (vxw, vyw, vzw) = getWindV ( vx,  vy,  vz) zw
humSkyZone n lat t h (Dry _)          z za zb zn zs ze zw = ((h*specificheatofair)+newh+windh)/(specificheatofair+2)
  where maxh = min 1.0 (max 0.1 ((t) / 24.0))
        newh = min hummin maxh
        windh = max hummin (min 1.0 ((hn+hs+he+hw)/4.0))
        hummin = 0.5+(0.5*(cos (7*pi*(lat/((fromIntegral gridh))))))
        hn   = (1+((-vyn)/momentumofair))*(getSkyH h zn)
        hs   = (1+(vys/momentumofair))*(getSkyH h zs)
        he   = (1+((-vxe)/momentumofair))*(getSkyH h ze)
        hw   = (1+(vxw/momentumofair))*(getSkyH h zw)
        ( vx,  vy,  vz) = getWindV (0.0, 0.0, 0.0) z
        (vxa, vya, vza) = getWindV ( vx,  vy,  vz) za
        (vxb, vyb, vzb) = getWindV ( vx,  vy,  vz) zb
        (vxn, vyn, vzn) = getWindV ( vx,  vy,  vz) zn
        (vxs, vys, vzs) = getWindV ( vx,  vy,  vz) zs
        (vxe, vye, vze) = getWindV ( vx,  vy,  vz) ze
        (vxw, vyw, vzw) = getWindV ( vx,  vy,  vz) zw

getOTemp :: OceanZone -> Float
getOTemp (Solid t)               = t
getOTemp (Ice   t)               = t
getOTemp (OceanZone t _ _ _ _ _) = t

formatWind :: Int -> [Sky] -> (Int, Int) -> String
formatWind n ss (x, y) = getWind n s x y
  where s = (ss !! (x+gridw*y))

getWind :: Int -> Sky -> Int -> Int -> String
getWind 1     (Sky lt  _  _  _  _) x y = "Low Tropospheric Wind: "    ++ (getZoneWindMaybe lt)
getWind 2000  (Sky  _ mt  _  _  _) x y = "Mid Tropospheric Wind: "    ++ (getZoneWindMaybe mt)
getWind 8000  (Sky  _  _ ht  _  _) x y = "High Tropospheric Wind: "   ++ (getZoneWindMaybe ht)
getWind 16000 (Sky  _  _  _ ls  _) x y = "Low Stratospheric Wind: "   ++ (getZoneWindMaybe ls)
getWind 24000 (Sky  _  _  _  _ hs) x y = "High Stratospheric Wind: "  ++ (getZoneWindMaybe hs)

getHumMaybe :: SkyZone -> String
getHumMaybe s = case (getHum s) of Nothing -> "Dry Land..."
                                   Just t  -> " Humidity: " ++ (showFloatFoReal (roundTo precision (hum s)))

getHum :: SkyZone -> Maybe String
getHum (Land _)              = Nothing
getHum (SkyZone _ _ h _ _ _) = Just (show h)

getZoneWindMaybe :: SkyZone -> String
getZoneWindMaybe s = case (getZoneWind s) of Nothing -> "Below Ground..."
                                             Just t  -> ((showXYZ(mapXYZ showFloatFoReal t)) ++ " Pressure: " ++ (showFloatFoReal np))
  where np = roundTo precision (bar s)

getZoneWind :: SkyZone -> Maybe (Float, Float, Float)
getZoneWind (Land _)                 = Nothing
getZoneWind (SkyZone _ _ _ vx vy vz) = Just (nvx, nvy, nvz)
  where nvx = roundTo precision vx
        nvy = roundTo precision vy
        nvz = roundTo precision vz

formatHumidity :: Int -> [Sky] -> (Int, Int) -> String
formatHumidity n ss (x, y) = getHumidity n s x y
  where s = (ss !! (x+gridw*y))

getHumidity :: Int -> Sky -> Int -> Int -> String
getHumidity 1     (Sky lt  _  _  _  _) x y = getHumMaybe lt
getHumidity 2000  (Sky  _ mt  _  _  _) x y = getHumMaybe mt
getHumidity 8000  (Sky  _  _ ht  _  _) x y = getHumMaybe ht
getHumidity 16000 (Sky  _  _  _ ls  _) x y = getHumMaybe ls
getHumidity 24000 (Sky  _  _  _  _ hs) x y = getHumMaybe hs
