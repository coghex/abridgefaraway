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

generateZone :: State -> Zone
generateZone state = genZone state x y zc conts seeds rands nconts
  where (x, y) = stateCursor state
        zc     = take (zoneh*zonew) (repeat 1)
        conts  = stateConts  state
        seeds  = stateSeeds  state
        rands  = stateRands  state
        nconts = stateNConts state

genZone :: State -> Int -> Int -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> Zone
genZone state x y zc conts seeds rands nconts = Zone { grid = g0
                                                     , cont = zoneconts
                                                     , elev = initZoneBlurElev x y state zoneconts zoneelev e conts seeds rands nconts
                                                     , mapx = x
                                                     , mapy = y
                                                     , camx = 0.0
                                                     , camy = 0.0
                                                     , camz = 0
                                                     , curx = round $ fromIntegral(zonew)/2.0
                                                     , cury = round $ fromIntegral(zoneh)/2.0
                                                     }
  where zoneconts        = zc1
        zoneelev         = take (zoneh*zonew) (repeat 0.0)
        zc1              = initZoneCont state x y zc conts seeds rands nconts
        (en, es, ee, ew) = cardinalsXY x y (stateElev state)
        enn              = quot (en+e) 2
        esn              = quot (es+e) 2
        een              = quot (ee+e) 2
        ewn              = quot (ew+e) 2
        e                = tapZoneGrid x y (stateElev state)
        g0               = initZoneGrid state 0

blurZone :: State -> [Float] -> Int -> [Float]
blurZone state elev n = elev

initZoneBlurElev :: Int -> Int -> State -> [Int] -> [Float] -> Int -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Float]
initZoneBlurElev x0 y0 state zc ze e l k j i = do
  let e1 = elevZone x0 y0 state zc ze l k j i
  blurZone state e1 erosion

elevZone :: Int -> Int -> State -> [Int] -> [Float] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Float]
elevZone x0 y0 state zc elev []     []     []     _ = elev
elevZone x0 y0 state zc elev _      _      _      0 = elev
elevZone x0 y0 state zc elev (l:ls) (k:ks) (j:js) i = do
  let x = findZoneElev x0 y0 state i (fst l) (snd l) zc elev k j
  elevZone x0 y0 state zc x ls ks js (i-1)

findZoneElev :: Int -> Int -> State -> Int -> Int -> Int -> [Int] -> [Float] -> [(Int, Int)] -> [(Int, Int)] -> [Float]
findZoneElev _  _  _     _ _ _ _  e []     []     = e
findZoneElev x0 y0 state c x y zc e (k:ks) (j:js) = do
  let newzc = expandZone zc
      newe  = expandZone e
      e0    = parMap rpar (elevZoneRow x0 y0 state newzc c (fst k) (snd k) (fst j) (snd j)) newe
      e1    = stripGrid e0
      e2    = flattenGrid e1
  findZoneElev x0 y0 state c x y zc e2 ks js

elevZoneRow :: Int -> Int -> State -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> ([(Float, Int)], Int) -> ([(Float, Int)], Int)
elevZoneRow x0 y0 state zc c w x y z (t1, t2) = (map (elevZoneTile x0 y0 state zc c t2 w x y z) t1, t2)

elevZoneTile :: Int -> Int -> State -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> (Float, Int) -> (Float, Int)
elevZoneTile x0 y0 state c e j w x y z (t, i) = ((elevOfZone dist t i j c), i)
  where dist = zoneDistance x0 y0 i j w x y z t

elevOfZone :: Float -> Float -> Int -> Int -> [([(Int, Int)], Int)] -> Float
elevOfZone dist t x y c = elevOfZoneSpot dist t typ
  where typ = (fst $ (fst (c !! y)) !! x)

elevOfZoneSpot :: Float -> Float -> Int -> Float
elevOfZoneSpot dist t 1 = avgZoneElev t $ normZoneElev dist 1 4
elevOfZoneSpot dist t 2 = avgZoneElev t $ -(normZoneElev dist 40 80)
elevOfZoneSpot dist t 3 = avgZoneElev t $ normZoneElev dist 5 6
elevOfZoneSpot dist t 4 = avgZoneElev t $ normZoneElev dist 5 40
elevOfZoneSpot dist t 5 = avgZoneElev t $ normZoneElev dist 20 100
elevOfZoneSpot dist t 6 = avgZoneElev t $ normZoneElev dist 12 40
elevOfZoneSpot dist t typ = 0

normZoneElev :: Float -> Int -> Int -> Float
normZoneElev x min max
  | (x < 0.1) = (min'-max')/2.0
  | otherwise = (((max' - min'))/(peaklevel))*x + min'
  where min' = fromIntegral min
        max' = fromIntegral max

avgZoneElev :: Float -> Float -> Float
avgZoneElev x y = x + (vigor'*y)
  where vigor' = fromIntegral vigor

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
  | (randstate == 1) && (zoneDistance x y i j w x0         y0         z0 t' <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w (x0+gridw) y0         z0 t' <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w x0         (y0+gridh) z0 t' <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w (x0-gridw) y0         z0 t' <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w x0         (y0-gridh) z0 t' <= maxdist)           = (randstate, i)
  | (randstate > 6) || (randstate < 2)                                                            = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         y0         z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0+gridw) y0         z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0+gridw) z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0-gridw) y0         z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0-gridw) z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         y0         z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0+gridw) y0         z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0+gridh) z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0-gridw) y0         z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0-gridh) z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 5) && (zoneDistance x y i j w x0         y0         z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w (x0+gridw) y0         z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w x0         (y0+gridh) z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w (x0-gridw) y0         z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w x0         (y0-gridh) z0 t' < maxdist-cfudge)     = (t, i)
  | zoneDistance x y i j w x0         y0         z0 t' <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w (x0+gridw) y0         z0 t' <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w x0         (y0+gridh) z0 t' <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w (x0-gridw) y0         z0 t' <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w x0         (y0-gridh) z0 t' <= maxdist                                 = (randstate, i)
  | otherwise                                                                                    = (t, i)
  where
    randstate = (stateTypes state) !! c
    maxdist   = 1000.0 * fromIntegral(((stateSizes state) !! c))
    cfudge    = 2.0 * fromIntegral(((stateRangeRands state) !! (c)) - minnconts)
    t'        = fromIntegral(t)

formatZoneElev :: [Float] -> (Int, Int) -> String
formatZoneElev e (x, y) = "Elev: " ++ (showFloatFoReal $ roundTo precision (getZoneElev e x y))

getZoneElev :: [Float] -> Int -> Int -> Float
getZoneElev e0 x y = do
  let elev = e0 !! (x+(zonew*y))
  elev - sealevel