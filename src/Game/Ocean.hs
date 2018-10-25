module Game.Ocean where

import Numeric (showFFloat)
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
  | ((abs vx) < currentslevel) && (vy > currentslevel)       = withTextures2D [(texs!!17)] $ drawOceanCurrentsTileTex n vy                       vz texs x y
  | (vx > currentslevel)       && (vy > currentslevel)       = withTextures2D [(texs!!18)] $ drawOceanCurrentsTileTex n ((vx+vy)/(sqrt 2))       vz texs x y
  | (vx < (-currentslevel))    && (vy > currentslevel)       = withTextures2D [(texs!!16)] $ drawOceanCurrentsTileTex n (((-vx)+vy)/(sqrt 2))    vz texs x y
  | ((abs vx) < currentslevel) && (vy < (-currentslevel))    = withTextures2D [(texs!!13)] $ drawOceanCurrentsTileTex n (-vy)                    vz texs x y
  | (vx > currentslevel)       && (vy < (-currentslevel))    = withTextures2D [(texs!!20)] $ drawOceanCurrentsTileTex n ((vx+(-vy))/(sqrt 2))    vz texs x y
  | (vx < (-currentslevel))    && (vy < (-currentslevel))    = withTextures2D [(texs!!14)] $ drawOceanCurrentsTileTex n (((-vx)+(-vy))/(sqrt 2)) vz texs x y
  | (vx > currentslevel)       && ((abs vy) < currentslevel) = withTextures2D [(texs!!19)] $ drawOceanCurrentsTileTex n vx                       vz texs x y
  | (vx < (-currentslevel))    && ((abs vy) < currentslevel) = withTextures2D [(texs!!15)] $ drawOceanCurrentsTileTex n (-vy)                    vz texs x y
  | otherwise                                                = withTextures2D [(texs!!1)]  $ drawVZTile vz texs x y

drawOceanCurrentsBackgroundTile :: [GL.TextureObject] -> Int -> Int -> OceanZone -> IO ()
drawOceanCurrentsBackgroundTile texs x y (Solid _)                = withTextures2D [(texs!!10)] $ drawNullTile texs x y
drawOceanCurrentsBackgroundTile texs x y (OceanZone _ _ _ _ _ vz) = withTextures2D [(texs!!1)] $ drawVZTile vz texs x y

drawOceanCurrentsTileTex :: Int -> Float -> Float -> [GL.TextureObject] -> Int -> Int -> IO ()
drawOceanCurrentsTileTex 1    size vz texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  glColor3f vz 1.0 1.0
  drawOceanSquare
  GL.depthFunc GL.$= Just GL.Lequal
  where currentszoom = min 1 (max (size*100) 0)
drawOceanCurrentsTileTex 200  size vz texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  glColor3f vz 1.0 1.0
  drawOceanSquare
  GL.depthFunc GL.$= Just GL.Lequal
  where currentszoom = min 1 (max (size*100) 0)
drawOceanCurrentsTileTex 1000 size vz texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  glColor3f vz 1.0 1.0
  drawOceanSquare
  GL.depthFunc GL.$= Just GL.Lequal
  where currentszoom = min 1 (max (size*100) 0)
drawOceanCurrentsTileTex n size vz texs x y = do
  glLoadIdentity
  GL.depthFunc GL.$= Nothing
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glScalef (currentszoom) (currentszoom) (currentszoom)
  glColor3f vz 1.0 1.0
  drawOceanSquare
  GL.depthFunc GL.$= Just GL.Lequal
  where currentszoom = min 1 (max (size*100) 0)

drawVZTile :: Float -> [GL.TextureObject] -> Int -> Int -> IO ()
drawVZTile vz texs x y = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glColor3f vz 1.0 1.0
  drawOceanSquare

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
                                            Just t  -> glColor3f (((t+2)/18.0)-1) (1.0 - (abs (((t+2)-18.0)/18.0))) (1.0-((t+2)/18.0))
            200  -> case (getZoneTemp m) of Nothing -> glColor3f 1.0 1.0 1.0
                                            Just t  -> glColor3f (((t+2)/8.0)-1) (1.0 - (abs (((t+2)-8.0)/8.0))) (1.0-((t+2)/8.0))
            1000 -> case (getZoneTemp b) of Nothing -> glColor3f 1.0 1.0 1.0
                                            Just t  -> glColor3f (((t+2)/8.0)-1) (1.0 - (abs (((t+2)-4.0)/8.0))) (1.0-((t+2)/8.0))
            4000 -> case (getZoneTemp a) of Nothing -> glColor3f 1.0 1.0 1.0
                                            Just t  -> glColor3f (((t+2)/8.0)-1) (1.0 - (abs (((t+2)-3.0)/8.0))) (1.0-((t+2)/8.0))
            6000 -> case (getZoneTemp h) of Nothing -> glColor3f 1.0 1.0 1.0
                                            Just t  -> glColor3f (((t*2)/8.0)-1) (1.0 - (abs (((t+2)-8.0)/8.0))) (1.0-((t+2)/8.0))
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
initSeaTemp l 6000 y p = 4.0
initSeaTemp l n    y p = (18.0/(perml+1.0)) + (20.0/(perml+1.0)*(cos (2.0*pi*(lat)/(fromIntegral(gridh))))) - 2.0
  where lat   = abs ((fromIntegral(y)) - ((fromIntegral(gridh))/2.0))
        perml = fromIntegral(n)/100

initSeaPres :: Float -> Int -> Float
initSeaPres sal depth = 1.0+fromIntegral(depth)/360.0

initSeaSal :: Float
initSeaSal = 35.0

getZoneCurrents :: OceanZone -> Maybe (Float, Float, Float)
getZoneCurrents (Solid _)               = Nothing
getZoneCurrents (OceanZone _ _ _ x y z) = Just (nx, ny, nz)
  where nx = roundTo precision x
        ny = roundTo precision y
        nz = roundTo precision z

getZoneCurrentsMaybe :: OceanZone -> String
getZoneCurrentsMaybe o = case (getZoneCurrents o) of Nothing -> "Below Seafloor..."
                                                     Just t  -> ((showXYZ(mapXYZ showFloatFoReal t)) ++ " Pressure:" ++ (showFloatFoReal np))
  where np = roundTo precision (pres o)

getV :: (Float, Float, Float) -> OceanZone -> (Float, Float, Float)
getV (vx0, vy0, vz0) (Solid _)                  = (0.0, 0.0, 0.0)
getV (_,   _,   _)   (OceanZone _ _ _ vx vy vz) = (vx,  vy,  vz)

getP :: Float -> OceanZone -> Float
getP p0 (Solid _)               = p0+0.001
getP _  (OceanZone _ p _ _ _ _) = p

getT :: Float -> OceanZone -> Float
getT t0 (Solid _)               = t0
getT _  (OceanZone t _ _ _ _ _) = t

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

calcLight :: Float -> Float -> Float
calcLight l lat = l*(cos (pi*(lat/((fromIntegral gridh)))))

pvnrt :: Int -> Float -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> Float
--pvnrt 1   p t l z za zb zn zs ze zw = ((4.0*t)+(nt))/5.0
--  where nt = (((-2.0 + ((p)+((10.0*l))))*specificheatofwater)+tn+ts+tw+te+tb)/(specificheatofwater+5.0)
--        t0 = (getT 0.0 z)
--        tb = case ((getT t0 zb)>t0) of
--               True  -> (getT t0 zb)
--               False -> t0
--        tn = (getT t0  zn)
--        ts = (getT t0  zs)
--        te = (getT t0  ze)
--        tw = (getT t0  zw)
--        (vx,  vy,  vz)  = getV (0.0, 0.0, 0.0) z
--        (vxa, vya, vza) = getV (vx, vy, vz)    za
--        (vxb, vyb, vzb) = getV (vx, vy, vz)    zb
--        (vxn, vyn, vzn) = getV (vx, vy, vz)    zn
--        (vxs, vys, vzs) = getV (vx, vy, vz)    zs
--        (vxe, vye, vze) = getV (vx, vy, vz)    ze
--        (vxw, vyw, vzw) = getV (vx, vy, vz)    zw
pvnrt n   p t l z za zb zn zs ze zw = ((4.0*t)+(nt))/5.0
  where nt = (((-2.0 + (((fromIntegral(n)/1000.0))+(p/fromIntegral(n))+((20.0*l)/(fromIntegral(n))))))+(t*specificheatofwater)+tn+ts+tw+te+ta+tb)/(specificheatofwater+7.0)
        t0 = (getT 0.0 z)
        ta = case ((getT t0 za)<t0) of
               True  -> (1+((vza)/specificheatofwater))*(getT t0 za)
               False -> t0
        tb = case ((getT t0 zb)>t0) of
               True  -> (1+((-vzb)/specificheatofwater))*(getT t0 zb)
               False -> t0
        tn = (1+((-vyn)/specificheatofwater))*(getT t0  zn)
        ts = (1+((vys)/specificheatofwater))*(getT t0  zs)
        te = (1+((-vxe)/specificheatofwater))*(getT t0  ze)
        tw = (1+((vxw)/specificheatofwater))*(getT t0  zw)
        (vx,  vy,  vz)  = getV (0.0, 0.0, 0.0) z
        (vxa, vya, vza) = getV (vx, vy, vz)    za
        (vxb, vyb, vzb) = getV (vx, vy, vz)    zb
        (vxn, vyn, vzn) = getV (vx, vy, vz)    zn
        (vxs, vys, vzs) = getV (vx, vy, vz)    zs
        (vxe, vye, vze) = getV (vx, vy, vz)    ze
        (vxw, vyw, vzw) = getV (vx, vy, vz)    zw

pZone :: Int -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> Float
pZone n z za zb zn zs ze zw = (((1 + ((fromIntegral(n))/360.0) + (t0/36.0)))+(p0*momentumofwater)+pn+ps+pw+pe+pa+pb)/(momentumofwater+7.0)
  where p0 = (getP 0.0 z)
        pa = case (ta<t0) of
               True  -> (1+((vza)/momentumofwater))*((getP p0  za)-(fromIntegral(decreaseOceanZ(n))/360.0)+(fromIntegral(n)/360.0))
               False -> p0
        pb = case (tb>t0) of
               True  -> (1+((-vzb)/momentumofwater))*((getP p0  zb)-(fromIntegral(increaseOceanZ(n))/360.0)+(fromIntegral(n)/360.0))
               False -> p0
        pn = (1+((-vyn)/momentumofwater))*(getP p0  zn)
        ps = (1+(vys/momentumofwater))*(getP p0  zs)
        pe = (1+((-vxe)/momentumofwater))*(getP p0  ze)
        pw = (1+(vxw/momentumofwater))*(getP p0  zw)
        t0 = (getT 0.0 z)
        ta = (getT t0  za)
        tb = (getT t0  zb)
        tn = (getT t0  zn)
        ts = (getT t0  zs)
        te = (getT t0  ze)
        tw = (getT t0  zw)
        (vx,  vy,  vz)  = getV (0.0, 0.0, 0.0) z
        (vxa, vya, vza) = getV (vx, vy, vz)    za
        (vxb, vyb, vzb) = getV (vx, vy, vz)    zb
        (vxn, vyn, vzn) = getV (vx, vy, vz)    zn
        (vxs, vys, vzs) = getV (vx, vy, vz)    zs
        (vxe, vye, vze) = getV (vx, vy, vz)    ze
        (vxw, vyw, vzw) = getV (vx, vy, vz)    zw
        nb = fromIntegral(increaseOceanZ n)
     

calcCurrentsV :: Int -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> OceanZone -> (Float, Float, Float)
calcCurrentsV n l lat z za zb zn zs ze zw = (nvx, nvy, nvz)
  where nvx = ((pw - p0) - (pe - p0))
        nvy = ((ps - p0) - (pn - p0))
        nvz = ((pb - p0) - (pa - p0))
        p0  = getP 0.0 z
        pa  = (getP p0  za)-(fromIntegral(decreaseOceanZ(n))/360.0)+(fromIntegral(n)/360.0)
        pb  = (getP p0  zb)-(fromIntegral(increaseOceanZ(n))/360.0)+(fromIntegral(n)/360.0)
        pn  = getP p0  zn
        ps  = getP p0  zs
        pe  = getP p0  ze
        pw  = getP p0  zw

eqSeaMaybe :: Int -> Float -> Float -> OceanZone -> OceanZone -> OceanZone -> Ocean -> Ocean -> Ocean -> Ocean -> Maybe OceanZone
eqSeaMaybe n    l lat z za zb on os oe ow    = case (z) of
                                                 Solid _                     -> Nothing
                                                 OceanZone t p s tvx tvy tvz -> Just ( OceanZone { temp = pvnrt n p t norml z za zb zn zs ze zw
                                                                                                 , pres = pZone n z za zb zn zs ze zw
                                                                                                 , sal  = s
                                                                                                 , vx   = nvx
                                                                                                 , vy   = nvy
                                                                                                 , vz   = nvz
                                                                                                 })
  where  (nvx, nvy, nvz) = calcCurrentsV n l lat z za zb zn zs ze zw
         zn              = getZone n on
         zs              = getZone n os
         ze              = getZone n oe
         zw              = getZone n ow
         norml           = calcLight l lat

tempOcean :: [Ocean] -> [Float] -> [Ocean]
tempOcean o l = do
  let (on, os, oe, ow) = cardinals o
  let xys              = yList
  map eqOceanTemp (zip7 o on os oe ow l xys)
  --parMap rpar eqOceanTemp (zip7 o on os oe ow l xys)

eqOceanTemp :: (Ocean, Ocean, Ocean, Ocean, Ocean, Float, Int) -> Ocean
eqOceanTemp ((Dry _), on, os, oe, ow, l, y)         = Dry 1
eqOceanTemp ((Sea e m b a h), on, os ,oe, ow, l, y) = Sea { epipelagic    = eqSeaTemp 1    l lat e (Solid 1) m            on os oe ow
                                                          , mesopelagic   = eqSeaTemp 200  l lat m e         b            on os oe ow
                                                          , bathypelagic  = eqSeaTemp 1000 l lat b m         a            on os oe ow
                                                          , abyssopelagic = eqSeaTemp 4000 l lat a b         h            on os oe ow
                                                          , hadopelagic   = eqSeaTemp 6000 l lat h a         (Solid 6000) on os oe ow
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
