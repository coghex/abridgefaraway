module Game.Ocean where

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar)
import Game.Map
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
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

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

drawOcean :: State -> [GL.TextureObject] -> IO ()
drawOcean state texs = do
  let onew = expandGrid $ stateOceans state
      n    = stateOceanTempZ state
  resequence_ (map (drawOceanRow n texs) onew)
  glFlush

drawOceanRow :: Int -> [GL.TextureObject] -> ([(Ocean, Int)], Int) -> IO ()
drawOceanRow n texs (a, b) = resequence_ (map (drawOceanSpot n texs b) a)

drawOceanSpot :: Int -> [GL.TextureObject] -> Int -> (Ocean, Int) -> IO ()
drawOceanSpot n texs y (o, x) = withTextures2D [(texs!!10)] $ drawOceanTile n texs x y o

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

getZoneTemp :: OceanZone -> Maybe Float
getZoneTemp (Solid _)               = Nothing
getZoneTemp (OceanZone t p s x y z) = Just t

getZoneTempForSure :: OceanZone -> Float
getZoneTempForSure (Solid _)               = 0
getZoneTempForSure (OceanZone t p s x y z) = t

getZoneTempMaybe :: OceanZone -> String
getZoneTempMaybe o = case (getZoneTemp o) of Nothing -> "Below Seafloor..."
                                             Just t  -> show t

getSeaTemp :: Int -> Ocean -> Int -> Int -> String
getSeaTemp 1    (Sea e m b a h) x y = "Epipelagic Temp: " ++ (show (getZoneTempMaybe e))
getSeaTemp 200  (Sea e m b a h) x y = "Mesopelagic Temp: " ++ (show (getZoneTempMaybe m))
getSeaTemp 1000 (Sea e m b a h) x y = "Bathypelagic Temp: " ++ (show (getZoneTempMaybe b))
getSeaTemp 4000 (Sea e m b a h) x y = "Abyssopelagic Temp: " ++ (show (getZoneTempMaybe a))
getSeaTemp 6000 (Sea e m b a h) x y = "Hadopelagic Temp: " ++ (show (getZoneTempMaybe h))
getSeaTemp n (Dry _) x y            = "Dry..."

formatOceanTemp :: Int -> [Ocean] -> (Int, Int) -> String
formatOceanTemp n os (x, y) = (getSeaTemp n o x y)
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
  | ((getZoneTempForSure e) < -2.1) = 11
  | otherwise                       = g
