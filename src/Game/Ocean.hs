module Game.Ocean where

import Data.List
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar)
import Game.Map
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import GLUtil.Textures
import qualified Graphics.UI.GLFW as GLFW

import Game.Settings
import Game.Data
import Game.State
import Game.Map

theGreatSeas :: [Int] -> [Int] -> [Float] -> [Ocean]
theGreatSeas grid elev light = do
  let g0 = expandGrid grid
      o1 = parMap rpar (newOceanRow elev light) g0
      o2 = stripGrid o1
  flattenGrid o2

-- turns a sequence of IO actions into a single IO action by monad magic
resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

-- draws the ocean temperatures
drawOcean :: State -> [GL.TextureObject] -> IO ()
drawOcean state texs = do
  let onew = expandGrid $ stateOceans state
      n    = stateOceanTempZ state
  resequence_ (map (drawOceanRow n texs) onew)
  glFlush

-- draws the ocean currents
drawOceanCurrents :: State -> [GL.TextureObject] -> IO ()
drawOceanCurrents state texs = do
  let onew = expandGrid $ stateOceans state
      n    = stateOceanCurrentsZ state
  resequence_ ((map (drawOceanCurrentsBackgroundRow n texs) onew) ++ (map (drawOceanCurrentsRow n texs) onew))
  glFlush

drawOceanRow :: Int -> [GL.TextureObject] -> ([(Ocean, Int)], Int) -> IO ()
drawOceanRow n texs (a, b) = resequence_ (map (drawOceanSpot n texs b) a)

drawOceanCurrentsRow :: Int -> [GL.TextureObject] -> ([(Ocean, Int)], Int) -> IO ()
drawOceanCurrentsRow n texs (a, b) = resequence_ (map (drawOceanCurrentsSpot n texs b) a)

drawOceanCurrentsBackgroundRow :: Int -> [GL.TextureObject] -> ([(Ocean, Int)], Int) -> IO ()
drawOceanCurrentsBackgroundRow n texs (a, b) = resequence_ (map (drawOceanCurrentsBackgroundSpot n texs b) a)

drawOceanSpot :: Int -> [GL.TextureObject] -> Int -> (Ocean, Int) -> IO ()
drawOceanSpot n texs y (o, x) = withTextures2D [(texs!!10)] $ drawOceanTile n texs x y o

drawOceanCurrentsSpot :: Int -> [GL.TextureObject] -> Int -> (Ocean, Int) -> IO ()
drawOceanCurrentsSpot n    texs y ((Dry _),         x) = withTextures2D [(texs!!10)] $ drawNullTile texs x y
drawOceanCurrentsSpot 1    texs y ((Sea e _ _ _ _), x) = drawOceanCurrentsTile 1    texs x y e
drawOceanCurrentsSpot 200  texs y ((Sea _ m _ _ _), x) = drawOceanCurrentsTile 200  texs x y m
drawOceanCurrentsSpot 1000 texs y ((Sea _ _ b _ _), x) = drawOceanCurrentsTile 1000 texs x y b
drawOceanCurrentsSpot 4000 texs y ((Sea _ _ _ a _), x) = drawOceanCurrentsTile 4000 texs x y a
drawOceanCurrentsSpot 6000 texs y ((Sea _ _ _ _ h), x) = drawOceanCurrentsTile 6000 texs x y h

drawOceanCurrentsBackgroundSpot :: Int -> [GL.TextureObject] -> Int -> (Ocean, Int) -> IO ()
drawOceanCurrentsBackgroundSpot n    texs y ((Dry _),         x) = withTextures2D [(texs!!10)] $ drawNullTile texs x y
drawOceanCurrentsBackgroundSpot 1    texs y ((Sea e _ _ _ _), x) = drawOceanCurrentsBackgroundTile texs x y e
drawOceanCurrentsBackgroundSpot 200  texs y ((Sea _ m _ _ _), x) = drawOceanCurrentsBackgroundTile texs x y m
drawOceanCurrentsBackgroundSpot 1000 texs y ((Sea _ _ b _ _), x) = drawOceanCurrentsBackgroundTile texs x y b
drawOceanCurrentsBackgroundSpot 4000 texs y ((Sea _ _ _ a _), x) = drawOceanCurrentsBackgroundTile texs x y a
drawOceanCurrentsBackgroundSpot 6000 texs y ((Sea _ _ _ _ h), x) = drawOceanCurrentsBackgroundTile texs x y h

drawOceanCurrentsTile :: Int -> [GL.TextureObject] -> Int -> Int -> OceanZone -> IO ()
drawOceanCurrentsTile n texs x y (Solid _ )                  = withTextures2D [(texs!!10)] $ drawNullTile texs x y
drawOceanCurrentsTile n texs x y (OceanZone _ _ _ vx vy vz)
  | ((abs vx) < currentslevel) && (vy > currentslevel)       = withTextures2D [(texs!!17)] $ drawOceanCurrentsTileTex n vy                texs x y
  | (vx > currentslevel)       && (vy > currentslevel)       = withTextures2D [(texs!!18)] $ drawOceanCurrentsTileTex n ((vx*vy)/2)       texs x y
  | (vx < (-currentslevel))    && (vy > currentslevel)       = withTextures2D [(texs!!16)] $ drawOceanCurrentsTileTex n (((-vx)*vy)/2)    texs x y
  | ((abs vx) < currentslevel) && (vy < (-currentslevel))    = withTextures2D [(texs!!13)] $ drawOceanCurrentsTileTex n (-vy)             texs x y
  | (vx > currentslevel)       && (vy < (-currentslevel))    = withTextures2D [(texs!!20)] $ drawOceanCurrentsTileTex n ((vx*(-vy))/2)    texs x y
  | (vx < (-currentslevel))    && (vy < (-currentslevel))    = withTextures2D [(texs!!14)] $ drawOceanCurrentsTileTex n (((-vx)*(-vy))/2) texs x y
  | (vx > currentslevel)       && ((abs vy) < currentslevel) = withTextures2D [(texs!!19)] $ drawOceanCurrentsTileTex n vx                texs x y
  | (vx < (-currentslevel))    && ((abs vy) < currentslevel) = withTextures2D [(texs!!15)] $ drawOceanCurrentsTileTex n (-vy)             texs x y
  | otherwise                                                = withTextures2D [(texs!!1)]  $ drawNullTile texs x y

drawOceanCurrentsBackgroundTile :: [GL.TextureObject] -> Int -> Int -> OceanZone -> IO ()
drawOceanCurrentsBackgroundTile texs x y (Solid _)               = withTextures2D [(texs!!10)] $ drawNullTile texs x y
drawOceanCurrentsBackgroundTile texs x y (OceanZone _ _ _ _ _ _) = withTextures2D [(texs!!1)] $ drawNullTile texs x y

drawOceanCurrentsTileTex :: Int -> Float -> [GL.TextureObject] -> Int -> Int -> IO ()
drawOceanCurrentsTileTex 1 size texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  glColor3f 1.0 1.0 1.0
  drawOceanSquare
  GL.depthFunc GL.$= Just GL.Lequal
  where currentszoom = min 1 (max size 0)
drawOceanCurrentsTileTex n size texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  glColor3f 1.0 1.0 1.0
  drawOceanSquare
  GL.depthFunc GL.$= Just GL.Lequal
  where currentszoom = min 1 (max (size/100) 0)

drawNullTile :: [GL.TextureObject] -> Int -> Int -> IO ()
drawNullTile texs x y = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glColor3f 1.0 1.0 1.0
  drawOceanSquare

drawOceanTile :: Int -> [GL.TextureObject] -> Int -> Int -> Ocean -> IO ()
drawOceanTile n texs x y (Sea e m b a h) = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  case n of 1    -> case (getZoneTemp e) of Nothing -> glColor3f 1.0 1.0 1.0
                                            Just t  -> glColor3f ((t/16.0)-1) (1.0 - (abs ((t-16.0)/16.0))) (1.0-(t/16.0))
            200  -> case (getZoneTemp m) of Nothing -> glColor3f 1.0 1.0 1.0
                                            Just t  -> glColor3f ((t/10.0)-1) (1.0 - (abs ((t-10.0)/12.0))) (1.0-(t/10.0))
            1000 -> case (getZoneTemp b) of Nothing -> glColor3f 1.0 1.0 1.0
                                            Just t  -> glColor3f ((t/4.0)-1) (1.0 - (abs ((t-4.0)/4.0))) (1.0-(t/4.0))
            4000 -> case (getZoneTemp a) of Nothing -> glColor3f 1.0 1.0 1.0
                                            Just t  -> glColor3f ((t/3.0)-1) (1.0 - (abs ((t-3.0)/3.0))) (1.0-(t/3.0))
            6000 -> case (getZoneTemp h) of Nothing -> glColor3f 1.0 1.0 1.0
                                            Just t  -> glColor3f ((t/2.0)-1) (1.0 - (abs ((t-2.0)/2.0))) (1.0-(t/2.0))
  drawOceanSquare
drawOceanTile n texs x y (Dry _) = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glColor3f 1.0 1.0 1.0
  drawOceanSquare

drawOceanSquare :: IO ()
drawOceanSquare = do
  glBegin GL_QUADS
  glTexCoord2f   0    0
  glVertex3f   (-1) (-1)  1
  glTexCoord2f   1    0
  glVertex3f     1  (-1)  1
  glTexCoord2f   1    1
  glVertex3f     1    1   1
  glTexCoord2f   0    1
  glVertex3f   (-1)   1   1
  glEnd

newOceanRow :: [Int] -> [Float] -> ([(Int, Int)], Int) -> ([(Ocean, Int)], Int)
newOceanRow e l (g, y) = ((map (newOceanSpot y e l) g), y)

newOceanSpot :: Int -> [Int] -> [Float] -> (Int, Int) -> (Ocean, Int)
newOceanSpot y e l (g, x) = ((newOcean g (tapGrid e x y) (tapGrid l x y) y), x)

newOcean :: Int -> Int -> Float -> Int -> Ocean
newOcean 1  e l y = Sea { epipelagic    = newZone e l y 1    0 0 0 0
                        , mesopelagic   = newZone e l y 200  0 0 0 0
                        , bathypelagic  = newZone e l y 1000 0 0 0 0
                        , abyssopelagic = newZone e l y 4000 0 0 0 0
                        , hadopelagic   = newZone e l y 6000 0 0 0 0
                        }
newOcean 12 e l y = Sea { epipelagic    = newZone e l y 1    0 0 0 0
                        , mesopelagic   = newZone e l y 200  0 0 0 0
                        , bathypelagic  = newZone e l y 1000 0 0 0 0
                        , abyssopelagic = newZone e l y 4000 0 0 0 0
                        , hadopelagic   = newZone e l y 6000 0 0 0 0
                        }
newOcean 7  e l y = Sea { epipelagic    = newZone e l y 1    0 0 0 0
                        , mesopelagic   = newZone e l y 200  0 0 0 0
                        , bathypelagic  = newZone e l y 1000 0 0 0 0
                        , abyssopelagic = newZone e l y 4000 0 0 0 0
                        , hadopelagic   = newZone e l y 6000 0 0 0 0
                        }
newOcean g e l y = Dry g

newZone :: Int -> Float -> Int -> Int -> Float -> Float -> Float -> Float -> OceanZone
newZone e l y 1 sal vx vy vz                        = newOceanZone 1 e l y vx vy vz
newZone e l y n sal vx vy vz
  | ((round ((fromIntegral(e))-(sealevel))) > (-n)) = Solid 0
  | otherwise                                       = newOceanZone n e l y vx vy vz


newOceanZone :: Int -> Int -> Float -> Int -> Float -> Float -> Float -> OceanZone
newOceanZone n e l y vx vy vz = OceanZone { temp = initSeaTemp l n y newpres
                                          , pres = newpres
                                          , sal  = newsal
                                          , vx   = vx
                                          , vy   = vy
                                          , vz   = vz
                                          }
  where newpres = initSeaPres newsal n
        newsal  = initSeaSal

initSeaTemp :: Float -> Int -> Int -> Float -> Float
initSeaTemp l n y p = (18.0/(perml+1.0)) + (20.0/(perml+1.0)*(cos (2.0*pi*(lat)/(fromIntegral(gridh))))) - 2.0
  where lat   = abs ((fromIntegral(y)) - ((fromIntegral(gridh))/2.0))
        perml = fromIntegral(n)/100

initSeaPres :: Float -> Int -> Float
initSeaPres sal depth = sal*fromIntegral(depth)/360.0

initSeaSal :: Float
initSeaSal = 35.0

getZoneCurrents :: OceanZone -> Maybe (Float, Float, Float)
getZoneCurrents (Solid _)               = Nothing
getZoneCurrents (OceanZone _ _ _ x y z) = Just (x, y, z)

getZoneCurrentsMaybe :: OceanZone -> String
getZoneCurrentsMaybe o = case (getZoneCurrents o) of Nothing -> "Below Seafloor..."
                                                     Just t  -> ((show t) ++ " Pressure:" ++ (show (pres o)))

getZoneTemp :: OceanZone -> Maybe Float
getZoneTemp (Solid _)               = Nothing
getZoneTemp (OceanZone t _ _ _ _ _) = Just t

getZoneTempForSure :: OceanZone -> Float -> Float
getZoneTempForSure (Solid _)               ot = ot
getZoneTempForSure (OceanZone t _ _ _ _ _) ot = t

getZoneTempMaybe :: OceanZone -> String
getZoneTempMaybe o = case (getZoneTemp o) of Nothing -> "Below Seafloor..."
                                             Just t  -> show t
getZone :: Int -> Ocean -> OceanZone
getZone n    (Dry _)         = Solid n
getZone 1    (Sea e _ _ _ _) = e
getZone 200  (Sea _ m _ _ _) = m
getZone 1000 (Sea _ _ b _ _) = b
getZone 4000 (Sea _ _ _ a _) = a
getZone 6000 (Sea _ _ _ _ h) = h

tempEZone :: Float -> Float -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> Ocean -> Ocean -> Ocean -> Ocean -> Float -> Float -> Float -> Float
tempEZone l lat t p z za zb on os oe ow tvx tvy tvz = ((p*specificheatofwater*t) + (-tvy*tn) + (tvy*ts) + (-tvx*te) + (tvx*tw) + (tvz*tb))/(p*specificheatofwater+5.0) + (clarityofwater*(400.0/(lat+4))*l/(specificheatofwater))
  where tn = getZoneTempForSure (getZone 1 on) terratemp
        ts = getZoneTempForSure (getZone 1 os) terratemp
        te = getZoneTempForSure (getZone 1 oe) terratemp
        tw = getZoneTempForSure (getZone 1 ow) terratemp
        tb = getZoneTempForSure (zb)           terratemp
tempMZone :: Float -> Float -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> Ocean -> Ocean -> Ocean -> Ocean -> Float -> Float -> Float -> Float
tempMZone l lat t p z za zb on os oe ow tvx tvy tvz = ((p*specificheatofwater*t) + (-tvy*tn) + (tvy*ts) + (-tvx*te) + (tvx*tw) + (0.1*tvz*ta) + (0.9*tvz*tb))/(p*specificheatofwater+5.0) + (clarityofwater*(20.0/(lat+1))*l/(specificheatofwater))
  where tn = getZoneTempForSure (getZone 200 on) terratemp
        ts = getZoneTempForSure (getZone 200 os) terratemp
        te = getZoneTempForSure (getZone 200 oe) terratemp
        tw = getZoneTempForSure (getZone 200 ow) terratemp
        ta = getZoneTempForSure (za)             terratemp
        tb = getZoneTempForSure (zb)             terratemp
tempBZone :: Float -> Float -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> Ocean -> Ocean -> Ocean -> Ocean -> Float -> Float -> Float -> Float
tempBZone l lat t p z za zb on os oe ow tvx tvy tvz = ((p*specificheatofwater*(t + (0.00001*maxdensitytemp))) + (-tvy*tn) + (tvy*ts) + (-tvx*te) + (tvx*tw) + (0.1*tvz*ta) + (0.9*tvz*tb))/(1.00001*p*specificheatofwater+5.0) + (clarityofwater*(1.0/(lat+1))*l/(specificheatofwater))
  where tn = getZoneTempForSure (getZone 1000 on) terratemp
        ts = getZoneTempForSure (getZone 1000 os) terratemp
        te = getZoneTempForSure (getZone 1000 oe) terratemp
        tw = getZoneTempForSure (getZone 1000 ow) terratemp
        ta = getZoneTempForSure (za)              terratemp
        tb = getZoneTempForSure (zb)              terratemp
tempAZone :: Float -> Float -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> Ocean -> Ocean -> Ocean -> Ocean -> Float -> Float -> Float -> Float
tempAZone l lat t p z za zb on os oe ow tvx tvy tvz = ((p*specificheatofwater*(t + (0.01*maxdensitytemp))) + (-tvy*tn) + (tvy*ts) + (-tvx*te) + (tvx*tw) + (0.1*tvz*ta) + (0.9*tvz*tb))/(1.01*p*specificheatofwater+5.0) + (clarityofwater*(0.1/(lat+1))*l/(specificheatofwater))
  where tn = getZoneTempForSure (getZone 4000 on) terratemp
        ts = getZoneTempForSure (getZone 4000 os) terratemp
        te = getZoneTempForSure (getZone 4000 oe) terratemp
        tw = getZoneTempForSure (getZone 4000 ow) terratemp
        ta = getZoneTempForSure (za)              terratemp
        tb = getZoneTempForSure (zb)              terratemp
tempHZone :: Float -> Float -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> Ocean -> Ocean -> Ocean -> Ocean -> Float -> Float -> Float -> Float
tempHZone l lat t p z za zb on os oe ow tvx tvy tvz = ((p*specificheatofwater*(t + (0.1*maxdensitytemp))) + (-tvy*tn) + (tvy*ts) + (-tvx*te) + (tvx*tw) + (0.1*tvz*ta))/(1.1*p*specificheatofwater+4.1)
  where tn = getZoneTempForSure (getZone 6000 on) terratemp
        ts = getZoneTempForSure (getZone 6000 os) terratemp
        te = getZoneTempForSure (getZone 6000 oe) terratemp
        tw = getZoneTempForSure (getZone 6000 ow) terratemp
        ta = getZoneTempForSure (za)              terratemp
 
waterVMaybe :: Int -> Float -> Float -> Ocean -> Ocean -> Float -> Float -> Float
waterVMaybe n    t p (Dry _) (Dry _) tv tv2 = tv
waterVMaybe n    t p (Dry _) o2      tv tv2 = tv
waterVMaybe n    t p o1      (Dry _) tv tv2 = tv
waterVMaybe 1    t p o1      o2      tv tv2 = waterV 1    False t p (epipelagic o1)    (epipelagic o2)    tv tv2
waterVMaybe 200  t p o1      o2      tv tv2 = waterV 200  False t p (mesopelagic o1)   (mesopelagic o2)   tv tv2
waterVMaybe 1000 t p o1      o2      tv tv2 = waterV 1000 False t p (bathypelagic o1)  (bathypelagic o2)  tv tv2
waterVMaybe 4000 t p o1      o2      tv tv2 = waterV 4000 False t p (abyssopelagic o1) (abyssopelagic o2) tv tv2
waterVMaybe 6000 t p o1      o2      tv tv2 = waterV 6000 False t p (hadopelagic o1)   (hadopelagic o2)   tv tv2

waterV :: Int -> Bool -> Float -> Float -> OceanZone -> OceanZone -> Float -> Float -> Float
waterV n    z     t p (Solid _) (Solid _) tv tv2 = tv
waterV n    z     t p (Solid _) z2        tv tv2 = tv
waterV n    z     t p z1        (Solid _) tv tv2 = tv
waterV 1    True  t p z1        z2        tv tv2 = ((4.0*tv)+(tv2*(p1-p)))/5.0
  where p1 = (pres z1)/10.0
waterV 6000 True  t p z1        z2        tv tv2 = ((4.0*tv)+(tv2*(p2-p)))/5.0
  where p2 = (pres z2)
waterV n    z     t p z1        z2        tv tv2 = ((4.0*tv)+((p2-p)-(p1-p)))/5.0
  where p1 = (pres z1)
        p2 = (pres z2)

pvnrt :: Float -> Float -> Int -> Float
pvnrt p t n = 10.0 + ((4.0*p)+(np))/5.0
  where np = ((maxdensitytemp+6+t)*(fromIntegral(n))/6.0)

eqSeaMaybe :: Int -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> Ocean -> Ocean -> Ocean -> Ocean -> Maybe OceanZone
eqSeaMaybe 1    l lat z za zb on os oe ow    = case (z) of
                                                 Solid _                     -> Nothing
                                                 OceanZone t p s tvx tvy tvz -> Just ( OceanZone { temp = tempEZone l lat t p z za zb on os oe ow tvx tvy tvz
                                                                                                 , pres = pvnrt p t 1
                                                                                                 , sal  = s
                                                                                                 , vx   = waterVMaybe 1 t p ow oe tvx tvy
                                                                                                 , vy   = waterVMaybe 1 t p on os tvy tvx
                                                                                                 , vz   = waterV 1 True t p zb za tvz (-(max (abs tvx) (abs tvy)))
                                                                                                 })
eqSeaMaybe 200  l lat z za zb on os oe ow    = case (z) of
                                                 Solid _                     -> Nothing
                                                 OceanZone t p s tvx tvy tvz -> Just ( OceanZone { temp = tempMZone l lat t p z za zb on os oe ow tvx tvy tvz
                                                                                                 , pres = pvnrt p t 200
                                                                                                 , sal  = s
                                                                                                 , vx   = waterVMaybe 200 t p ow oe tvx tvy
                                                                                                 , vy   = waterVMaybe 200 t p on os tvy tvx
                                                                                                 , vz   = waterV 200 True t p zb za tvz (0.1)
                                                                                                 })
eqSeaMaybe 1000  l lat z za zb on os oe ow    = case (z) of
                                                 Solid _                     -> Nothing
                                                 OceanZone t p s tvx tvy tvz -> Just ( OceanZone { temp = tempBZone l lat t p z za zb on os oe ow tvx tvy tvz
                                                                                                 , pres = pvnrt p t 1000
                                                                                                 , sal  = s
                                                                                                 , vx   = waterVMaybe 1000 t p ow oe tvx tvy
                                                                                                 , vy   = waterVMaybe 1000 t p on os tvy tvx
                                                                                                 , vz   = waterV 1000 True t p zb za tvz (0.01)
                                                                                                 })
eqSeaMaybe 4000  l lat z za zb on os oe ow    = case (z) of
                                                 Solid _                     -> Nothing
                                                 OceanZone t p s tvx tvy tvz -> Just ( OceanZone { temp = tempAZone l lat t p z za zb on os oe ow tvx tvy tvz
                                                                                                 , pres = pvnrt p t 4000
                                                                                                 , sal  = s
                                                                                                 , vx   = waterVMaybe 4000 t p ow oe tvx tvy
                                                                                                 , vy   = waterVMaybe 4000 t p on os tvy tvx
                                                                                                 , vz   = waterV 4000 True t p zb za tvz (0.01)
                                                                                                 })
eqSeaMaybe 6000  l lat z za zb on os oe ow    = case (z) of
                                                 Solid _                     -> Nothing
                                                 OceanZone t p s tvx tvy tvz -> Just ( OceanZone { temp = tempHZone l lat t p z za zb on os oe ow tvx tvy tvz
                                                                                                 , pres = pvnrt p t 6000
                                                                                                 , sal  = s
                                                                                                 , vx   = waterVMaybe 6000 t p ow oe tvx tvy
                                                                                                 , vy   = waterVMaybe 6000 t p on os tvy tvx
                                                                                                 , vz   = waterV 6000 True t p zb za tvz (max (abs tvx) (abs tvy))
                                                                                                 })
eqSeaMaybe n    l lat z za zb on os oe ow    = case (z) of
                                                 Solid _                     -> Nothing
                                                 OceanZone t p s tvx tvy tvz -> Just ( OceanZone { temp = t
                                                                                                 , pres = p
                                                                                                 , sal  = s
                                                                                                 , vx   = tvx
                                                                                                 , vy   = tvy
                                                                                                 , vz   = tvz
                                                                                                 })

tempOcean :: [Ocean] -> [Float] -> [Ocean]
tempOcean o l = do
  let (on, os, oe, ow) = cardinals o
  let xys              = yList
  map eqOceanTemp (zip7 o on os oe ow l xys)
  --parMap rpar eqOceanTemp (zip7 o on os oe ow l xys)

eqOceanTemp :: (Ocean, Ocean, Ocean, Ocean, Ocean, Float, Int) -> Ocean
eqOceanTemp ((Dry _), on, os, oe, ow, l, y)         = Dry 1
eqOceanTemp ((Sea e m b a h), on, os ,oe, ow, l, y) = Sea { epipelagic    = eqSeaTemp 1    l lat e e m on os oe ow
                                                          , mesopelagic   = eqSeaTemp 200  l lat m e b on os oe ow
                                                          , bathypelagic  = eqSeaTemp 1000 l lat b m a on os oe ow
                                                          , abyssopelagic = eqSeaTemp 4000 l lat a b h on os oe ow
                                                          , hadopelagic   = eqSeaTemp 6000 l lat h a h on os oe ow
                                                          }
  where lat = abs (fromIntegral(y) - (fromIntegral(gridh)/2))

eqSeaTemp :: Int -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> Ocean -> Ocean -> Ocean -> Ocean -> OceanZone
eqSeaTemp n l lat z za zb on os oe ow = case (eqSeaMaybe n l lat z za zb on os oe ow) of
                                          Nothing          -> Solid n
                                          Just z           -> z

getSeaTemp :: Int -> Ocean -> Int -> Int -> String
getSeaTemp n    (Dry _)         x y = "Dry Land..."
getSeaTemp 1    (Sea e m b a h) x y = "Epipelagic Temp: "    ++ (getZoneTempMaybe e)
getSeaTemp 200  (Sea e m b a h) x y = "Mesopelagic Temp: "   ++ (getZoneTempMaybe m)
getSeaTemp 1000 (Sea e m b a h) x y = "Bathypelagic Temp: "  ++ (getZoneTempMaybe b)
getSeaTemp 4000 (Sea e m b a h) x y = "Abyssopelagic Temp: " ++ (getZoneTempMaybe a)
getSeaTemp 6000 (Sea e m b a h) x y = "Hadopelagic Temp: "   ++ (getZoneTempMaybe h)

formatOceanTemp :: Int -> [Ocean] -> (Int, Int) -> String
formatOceanTemp n os (x, y) = getSeaTemp n o x y
  where o = (os !! (x+gridw*y))

getSeaCurrents :: Int -> Ocean -> Int -> Int -> String
getSeaCurrents n    (Dry _)         x y = "Dry Land..."
getSeaCurrents 1    (Sea e m b a h) x y = "Epipelagic Currents: "    ++ (getZoneCurrentsMaybe e)
getSeaCurrents 200  (Sea e m b a h) x y = "Mesopelagic Currents: "   ++ (getZoneCurrentsMaybe m)
getSeaCurrents 1000 (Sea e m b a h) x y = "Bathypelagic Currents: "  ++ (getZoneCurrentsMaybe b)
getSeaCurrents 4000 (Sea e m b a h) x y = "Abyssopelagic Currents: " ++ (getZoneCurrentsMaybe a)
getSeaCurrents 6000 (Sea e m b a h) x y = "Hadopelagic Currents: "   ++ (getZoneCurrentsMaybe h)

formatOceanCurrents :: Int -> [Ocean] -> (Int, Int) -> String
formatOceanCurrents n os (x, y) = getSeaCurrents n o x y
  where o = (os !! (x+gridw*y))

decreaseOceanZ :: Int -> Int
decreaseOceanZ 1    = 1
decreaseOceanZ 200  = 1
decreaseOceanZ 1000 = 200
decreaseOceanZ 4000 = 1000
decreaseOceanZ 6000 = 4000
decreaseOceanZ x    = x

increaseOceanZ :: Int -> Int
increaseOceanZ 1    = 200
increaseOceanZ 200  = 1000
increaseOceanZ 1000 = 4000
increaseOceanZ 4000 = 6000
increaseOceanZ 6000 = 6000
increaseOceanZ x    = x

iceMap :: State -> Env -> [Int] -> [Ocean] -> [Int]
iceMap state env []     o      = []
iceMap state env (g:gs) (o:os) = (iceSpot g o) : (iceMap state env gs os)

iceSpot :: Int -> Ocean -> Int
iceSpot g (Dry _)                 = g
iceSpot g (Sea (Solid _) _ _ _ _) = g
iceSpot g (Sea e _ _ _ _)
  | ((getZoneTempForSure e 0) < -2.1) = 11
  | otherwise                         = g
