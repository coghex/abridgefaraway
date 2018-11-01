module Game.Zone where

import Control.Parallel.Strategies (parMap, rpar)
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import GLUtil.Textures
import Game.Data
import Game.State
import Game.Settings
import Game.Map
import Game.Draw
import Game.Sun


drawZoneElev :: State -> [[GL.TextureObject]] -> IO ()
drawZoneElev state texs = do
  let gnew               = expandZone currentZone
      currentZone        = elev (head (stateZones state))
      (camx, camy, camz) = getZoneCam (head (stateZones state))
      nulltex            = texs !! 10
  resequence_ (map (drawZoneElevRow nulltex camx camy camz) gnew)
  glFlush

drawZoneElevRow :: [GL.TextureObject] -> Float -> Float -> Int -> ([(Float, Int)], Int) -> IO ()
drawZoneElevRow texs camx camy camz (t1, t2) = resequence_ (map (drawZoneElevSpot texs camx camy camz t2) t1)

drawZoneElevSpot :: [GL.TextureObject] -> Float -> Float -> Int -> Int -> (Float, Int) -> IO ()
drawZoneElevSpot texs camx camy camz y (t, x) = withTextures2D texs $ drawZoneElevTile texs camx camy camz x y t

drawZoneElevTile :: [GL.TextureObject] -> Float -> Float -> Int -> Int -> Int -> Float -> IO ()
drawZoneElevTile texs camx camy camz x y t = do
  glLoadIdentity
  glTranslatef (2*((nx) - ((fromIntegral zonew)/2))) (2*((ny) - ((fromIntegral zoneh)/2))) (-zoom/4)
  glColor3f t t $ elevZoneOcean t
  drawSquare
  where nx = fromIntegral(x)+camx
        ny = fromIntegral(y)+camy

elevZoneOcean :: Float -> Float
elevZoneOcean x
  | x <= (sealevel/(peaklevel)) = 8
  | otherwise = x

drawZone :: State -> [[GL.TextureObject]] -> IO ()
drawZone state texs = do
  let zgs                = stateZones state
      mapxr              = mapx currzone
      mapyr              = mapy currzone
      sun                = tapGrid sunspots mapxr mapyr
      sunspots           = stateSunSpots state
      (camx, camy, camz) = getZoneCam currzone
      currzone           = head zgs
      zcnew              = expandZone $ (cont currzone)
      zgnew              = expandZone $ (grid currzone)
      zgzip              = zip zgnew zcnew
  resequence_ (map (drawZoneRow texs camx camy camz sun) zgzip)
  glFlush

drawZoneRow :: [[GL.TextureObject]] -> Float -> Float -> Int -> Float -> (([(Int, Int)], Int), ([(Int, Int)], Int)) -> IO ()
drawZoneRow texs camx camy camz sun ((a, y), (b, _)) = resequence_ (map (drawZoneSpot texs camx camy camz sun y) newzgzip)
  where newzgzip = zip a b

drawZoneSpot :: [[GL.TextureObject]] -> Float -> Float -> Int -> Float -> Int -> ((Int, Int), (Int, Int)) -> IO ()
drawZoneSpot texs camx camy camz sun y ((g, x), (c, _)) = withTextures2D tex $ drawZoneTile tex camx camy camz sun x y
  where tex = [((texs !! c) !! g)]

drawZoneCursor :: State -> [[GL.TextureObject]] -> IO ()
drawZoneCursor state texs = do
  let (cx, cy)           = getZoneCursor currzone
      zgs                = stateZones state
      mapxr              = mapx currzone
      mapyr              = mapy currzone
      sun                = tapGrid sunspots mapxr mapyr
      sunspots           = stateSunSpots state
      (camx, camy, camz) = getZoneCam currzone
      currzone           = head zgs
  withTextures2D (head texs) $ drawZoneTile (head texs) camx camy camz (sun) cx cy

drawZoneTile :: [GL.TextureObject] -> Float -> Float -> Int -> Float -> Int -> Int -> IO ()
drawZoneTile texs camx camy 0 sun x y = do
  glLoadIdentity
  glTranslatef (2*((nx) - ((fromIntegral zonew)/2))) (2*((ny) - ((fromIntegral zoneh)/2))) (-zoom/4)
  glColor3f t1 t2 t3
  drawSquare
  where
    t1 = 0.8*sun*sun + 0.2*sun
    t2 = 0.9*sun
    t3 = (log (sun+1))
    nx = fromIntegral(x)+camx
    ny = fromIntegral(y)+camy

getZoneCursor :: Zone -> (Int, Int)
getZoneCursor zg = (cx, cy)
  where cx = curx zg
        cy = cury zg

getZoneCam :: Zone -> (Float, Float, Int)
getZoneCam zg = (camxr, camyr, camzr)
  where camxr = camx zg
        camyr = camy zg
        camzr = camz zg

moveZoneCam :: Float -> Zone -> Card -> Zone
moveZoneCam n zone South
  | (y < (fromIntegral zoneh))   = moveCamZone zone (x, y+n)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone
moveZoneCam n zone North
  | (y > -(fromIntegral zoneh))       = moveCamZone zone (x, y-n)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone
moveZoneCam n zone East
  | (x > -(fromIntegral zonew))       = moveCamZone zone (x-n, y)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone
moveZoneCam n zone West
  | (x < (fromIntegral zonew))   = moveCamZone zone (x+n, y)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone

moveCamZone :: Zone -> (Float, Float) -> Zone
moveCamZone zone (x, y) = Zone { grid = (grid zone)
                               , cont = (cont zone)
                               , elev = (elev zone)
                               , mapx = (mapx zone)
                               , mapy = (mapy zone)
                               , camx = x
                               , camy = y
                               , camz = (camz zone)
                               , curx = (curx zone)
                               , cury = (cury zone) }

elevBlurZone :: State -> Int -> Int -> [Int] -> [Int]
elevBlurZone state x y zone = zone

generateZone :: State -> Zone
generateZone state = genZone state x y zc conts seeds rands nconts
  where (x, y) = stateCursor state
        zc     = take (zoneh*zonew) (repeat 1)
        conts  = stateConts  state
        seeds  = stateSeeds  state
        rands  = stateRands  state
        nconts = stateNConts state

genZone :: State -> Int -> Int -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> Zone
genZone state x y zc conts seeds rands nconts = Zone { grid = initZoneGrid state 0
                                                     , cont = zoneconts
                                                     , elev = initZoneElev state zc e en es ee ew
                                                     , mapx = x
                                                     , mapy = y
                                                     , camx = 0.0
                                                     , camy = 0.0
                                                     , camz = 0
                                                     , curx = round $ fromIntegral(zonew)/2.0
                                                     , cury = round $ fromIntegral(zoneh)/2.0
                                                     }
  where zoneconts        = zc0
        zc0              = elevBlurZone state x y zc1
        zc1              = initZoneCont state x y zc conts seeds rands nconts
        (en, es, ee, ew) = cardinalsXY x y (stateElev state)
        e                = tapZoneGrid x y (stateElev state)

initZoneElev :: State -> [Int] -> Int -> Int -> Int -> Int -> Int -> [Float]
initZoneElev state zc e en es ee ew = zc2
  where newz = expandZone zc
        zc0  = parMap rpar (seedZoneElevRow state e en es ee ew) newz
        zc1  = stripGrid zc0
        zc2  = flattenGrid zc1

seedZoneElevRow :: State -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Float, Int)], Int)
seedZoneElevRow state e en es ee ew (t1, t2) = (map (seedZoneElevSpot state ef enf esf eef ewf t2) t1, t2)
  where ef  = fromIntegral e
        enf = fromIntegral en
        esf = fromIntegral es
        eef = fromIntegral ee
        ewf = fromIntegral ew

seedZoneElevSpot :: State -> Float -> Float -> Float -> Float -> Float -> Int -> (Int, Int) -> (Float, Int)
seedZoneElevSpot state e en es ee ew j (t, i) = (zelev, i)
  where zelev  = ((diste*e)+(disten*en)+(distes*es)+(distee*ee)+(distew*ew))
        diste  = 1.0 - (sqrt((((x2 - x1)*(x2 - x1))/(zonewf)) + (((y2 - y1)*(y2 - y1))/(zonehf))))
        x1     = zonewf/2.0
        x2     = fromIntegral i
        y1     = zonehf/2.0
        y2     = fromIntegral j
        disten = 1.0 - (sqrt((((x2 - nx1)*(x2 - nx1))/(zonewf*zonewf)) + (((y2 - ny1)*(y2 - ny1))/(zonehf*zonehf))))
        nx1    = zonewf/2.0
        ny1    = -(zonehf/2.0)
        distes = 1.0 - (sqrt((((x2 - sx1)*(x2 - sx1))/(zonewf*zonewf)) + (((y2 - sy1)*(y2 - sy1))/(zonehf*zonehf))))
        sx1    = zonewf/2.0
        sy1    = (3.0*zonehf)/2.0
        distee = 1.0 - (sqrt((((x2 - ex1)*(x2 - ex1))/(zonewf*zonewf)) + (((y2 - ey1)*(y2 - ey1))/(zonehf*zonehf))))
        ex1    = (3.0*zonewf)/2.0
        ey1    = zonehf/2.0
        distew = 1.0 - (sqrt((((x2 - wx1)*(x2 - wx1))/(zonewf*zonewf)) + (((y2 - wy1)*(y2 - wy1))/(zonehf*zonehf))))
        wx1    = -(zonewf/2.0)
        wy1    = zonehf/2.0
        zonewf = fromIntegral zonew
        zonehf = fromIntegral zoneh

initZoneGrid :: State -> Int -> [Int]
initZoneGrid state n = take (zoneh*zonew) (repeat n)

initZoneCont :: State -> Int -> Int -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
initZoneCont state _ _ zg0 []     []     []     _ = zg0
initZoneCont state _ _ zg0 _      _      _      0 = zg0
initZoneCont state x y zg0 (l:ls) (k:ks) (j:js) i = do
  let zg = seedZoneCont state x y 0 i (fst l) (snd l) zg0 k j
  initZoneCont state x y zg ls ks js (i-1)

seedZoneCont :: State -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
seedZoneCont state _ _ _ _ _  _  zg []     []     = zg
seedZoneCont state x y i c x0 y0 zg (k:ks) (j:js) = do
  let nzg = expandZone zg
      zg0 = parMap rpar (seedZoneContRow state x y c i (fst k) (snd k) (fst j) (snd j)) nzg
      zg1 = stripGrid zg0
      zg2 = flattenGrid zg1
  seedZoneCont state x y (i+1) c x0 y0 zg2 ks js

seedZoneContRow :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedZoneContRow state x y i c w x0 y0 z0 (t1, t2) = (map (seedZoneContTile state x y i c t2 w x0 y0 z0) t1, t2)

seedZoneContTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedZoneContTile state x y it c j w x0 y0 z0 (t, i)
  | (randstate == 1) && (zoneDistance x y i j w x0         y0         z0 t <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w (x0+gridw) y0         z0 t <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w x0         (y0+gridh) z0 t <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w (x0-gridw) y0         z0 t <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w x0         (y0-gridh) z0 t <= maxdist)           = (randstate, i)
  | (randstate > 6) || (randstate < 2)                                                           = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         y0         z0 t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0+gridw) y0         z0 t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0+gridw) z0 t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0-gridw) y0         z0 t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0-gridw) z0 t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         y0         z0 t <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0+gridw) y0         z0 t <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0+gridh) z0 t <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0-gridw) y0         z0 t <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0-gridh) z0 t <= (4*maxdist))       = (randstate, i)
  | (randstate == 5) && (zoneDistance x y i j w x0         y0         z0 t < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w (x0+gridw) y0         z0 t < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w x0         (y0+gridh) z0 t < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w (x0-gridw) y0         z0 t < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w x0         (y0-gridh) z0 t < maxdist-cfudge)     = (t, i)
  | zoneDistance x y i j w x0         y0         z0 t <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w (x0+gridw) y0         z0 t <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w x0         (y0+gridh) z0 t <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w (x0-gridw) y0         z0 t <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w x0         (y0-gridh) z0 t <= maxdist                                 = (randstate, i)
  | otherwise                                                                                    = (t, i)
  where
    randstate = (stateTypes state) !! c
    maxdist   = 1000.0 * fromIntegral(((stateSizes state) !! c))
    cfudge    = 2.0 * fromIntegral(((stateRangeRands state) !! (c)) - minnconts)
