module Game.Ocean where

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
      o1 = zipWith3 newOceanRow elev light g0
      o2 = stripGrid o1
  flattenGrid o2

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

drawOcean :: State -> [GL.TextureObject] -> IO ()
drawOcean state texs = do
  let onew = expandGrid $ stateOceans state
  resequence_ (map (drawOceanRow texs) onew)
  glFlush

drawOceanRow :: [GL.TextureObject] -> ([(Ocean, Int)], Int) -> IO ()
drawOceanRow texs (a, b) = resequence_ (map (drawOceanSpot texs b) a)

drawOceanSpot :: [GL.TextureObject] -> Int -> (Ocean, Int) -> IO ()
drawOceanSpot texs y (o, x) = withTextures2D [(texs!!10)] $ drawOceanTile texs x y o

drawOceanTile :: [GL.TextureObject] -> Int -> Int -> Ocean -> IO ()
drawOceanTile texs x y (Sea e m b a h) = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glColor3f ot 0.4 1.0
  drawOceanSquare
  where ot = (getZoneTemp e)/33.0
drawOceanTile texs x y (Dry _) = do
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-zoom)
  glColor3f 0.0 0.0 0.0
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

newOceanRow :: Int -> Float -> ([(Int, Int)], Int) -> ([(Ocean, Int)], Int)
newOceanRow e l (g, y) = ((map (newOceanSpot e l y) g), y)

newOceanSpot :: Int -> Float -> Int -> (Int, Int) -> (Ocean, Int)
newOceanSpot e l y (g, x) = ((newOcean g e l y), x)

newOcean :: Int -> Int -> Float -> Int -> Ocean
newOcean 1 e l y  = Sea { epipelagic    = newZone e l y 1    0 0 0 0
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
newOcean 7 e l y  = Sea { epipelagic    = newZone e l y 1    0 0 0 0
                        , mesopelagic   = newZone e l y 200  0 0 0 0
                        , bathypelagic  = newZone e l y 1000 0 0 0 0
                        , abyssopelagic = newZone e l y 4000 0 0 0 0
                        , hadopelagic   = newZone e l y 6000 0 0 0 0
                        }
newOcean g e l y = Dry g

newZone :: Int -> Float -> Int -> Int -> Float -> Float -> Float -> Float -> OceanZone
newZone e l y n sal vx vy vz
  | (e < n)     = Solid 0
  | otherwise   = newOceanZone n e l y vx vy vz


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
initSeaTemp l n y p = (36.0) + 2
  where lat = abs ((fromIntegral(y)) - ((fromIntegral(gridh))/2))

initSeaPres :: Float -> Int -> Float
initSeaPres sal depth = sal*fromIntegral(depth)/360.0

initSeaSal :: Float
initSeaSal = 35.0

getZoneTemp :: OceanZone -> Float
getZoneTemp (Solid _) = 2
getZoneTemp (OceanZone t p s x y z) = t

getSeaTemp :: Int -> Ocean -> Int -> Int -> String
getSeaTemp 1 (Sea e m b a h) x y = show $ getZoneTemp e
getSeaTemp 1 (Dry _) x y         = "Dry..."

formatOceanTemp :: [Ocean] -> (Int, Int) -> String
formatOceanTemp os (x, y) = "Epipelagic Temp: " ++ (getSeaTemp 1 o x y)
  where o = (os !! (x+gridw*y))
