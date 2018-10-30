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
  glTranslatef (2*((nx) - ((fromIntegral zonew)/2))) (2*((ny) - ((fromIntegral zoneh)/2))) (-zoom)
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
moveZoneCam n zone North
  | (y < (fromIntegral zoneh))   = moveCamZone zone (x, y+n)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone
moveZoneCam n zone South
  | (y > 0.0)       = moveCamZone zone (x, y-n)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone
moveZoneCam n zone West
  | (x > 0.0)       = moveCamZone zone (x-n, y)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone
moveZoneCam n zone East
  | (x < (fromIntegral zonew))   = moveCamZone zone (x+n, y)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone

moveCamZone :: Zone -> (Float, Float) -> Zone
moveCamZone zone (x, y) = Zone { grid = (grid zone)
                               , cont = (cont zone)
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
genZone state x y zc conts seeds rands nconts = Zone { grid = initZoneGrid state 0
                                                     , cont = initZoneGrid state 1--initZoneCont state x y zc conts seeds rands nconts
                                                     , mapx = x
                                                     , mapy = y
                                                     , camx = 0.0
                                                     , camy = 0.0
                                                     , camz = 0
                                                     , curx = round $ fromIntegral(zonew)/2.0
                                                     , cury = round $ fromIntegral(zoneh)/2.0
                                                     }

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
seedZoneContRow state x y c i w x0 y0 z0 (t1, t2) = (map (seedZoneContTile state x y c i t2 w x0 y0 z0) t1, t2)

seedZoneContTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedZoneContTile state x y c it j w x0 y0 z0 (t, i)
  | (randstate==1) && (zoneDistance x y i j w x0 y0 z0 t <= maxdist)   = (randstate, i)
  | otherwise                                                          = (t, i)
  where
    randstate = (stateTypes state) !! c
    maxdist   = 1000 * ((stateSizes state) !! c)
    cfudge    = 2 * (((stateRangeRands state) !! c) - minnconts)
