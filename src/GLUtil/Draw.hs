module GLUtil.Draw where
-- the drawing of various screens is defined

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString.Lazy as BS
import qualified GLUtil.ABFA as GLFW
import Control.Parallel.Strategies (parMap, rpar)
import GLUtil.Textures
import GLUtil.ABFA
import GLUtil.Util
import ABFA.Game
import ABFA.State
import ABFA.Data
import ABFA.Map

-- draws rows one at a time, parmap wont work for this
drawWorld :: State -> Env -> IO ()
drawWorld state env = do
  let gnew     = expandGrid gridw gridh $ stateGrid state
      texs     = envWTex env
      settings = stateSettings state
      gridw    = settingGridW settings
      gridh    = settingGridH settings
      screenw  = settingScreenW settings
      screenh  = settingScreenH settings
  resequence_ (map (drawWorldRow texs screenw screenh gridw gridh) gnew)
  glFlush

-- draws each spot
drawWorldRow :: [GL.TextureObject] -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> IO ()
drawWorldRow texs screenw screenh gridw gridh (a, b) = resequence_ (map (drawWorldSpot texs screenw screenh gridw gridh b) a)

-- using the texture library
drawWorldSpot :: [GL.TextureObject] -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> IO ()
drawWorldSpot texs screenw screenh gridw gridh y (t, x) = withTextures2D [(texs!!t)] $ drawSceneTile texs screenw screenh gridw gridh x y t

-- glwork done to handle zoom and location of each tile
drawSceneTile :: [GL.TextureObject] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
drawSceneTile texs screenw screenh gridw gridh x y t = do
  glLoadIdentity
  glTranslatef nx ny nz-- (10.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (10.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  glColor3f 1.0 1.0 1.0
  drawSquare
  where
    (nx, ny, nz) = worldZoom x y screenw screenh gridw gridh
  
-- calculates the glTraslatef input value for world screen
worldZoom :: Int -> Int -> Int -> Int -> Int -> Int -> (Float, Float, Float)
worldZoom x y screenw screenh gridw gridh = (nx, ny, nz)
  where nx = (fromIntegral (2*x)) - (0.6*(fromIntegral (gridw))) --2*((fromIntegral x) - ((fromIntegral gridw)/2))
        ny = (fromIntegral (2*y)) - ((fromIntegral (gridh))) --2*((fromIntegral y) - ((fromIntegral gridh)/2))
        nz = -2.0*(fromIntegral (max gridw gridh))
        gridratio = (fromIntegral gridh) / (fromIntegral gridw)
--
-- calculates the glTraslatef input value for zone screen
zoneZoom :: Int -> Int -> Float -> Float -> Int -> Int -> Int -> Int -> Float -> (Float, Float, Float)
zoneZoom mapx mapy camx camy x y zonew zoneh zoom = (nx, ny, nz)
  -- first term is position in zone, second is the offset to fit the UI,
  -- third is where the zone is relative to the home zone
  where nx = (fromIntegral (2*x)) - (0.6*(fromIntegral (zonew))) + (fromIntegral(mapx*zonew)) - (camx)
        ny = (fromIntegral (2*y)) - ((fromIntegral (zoneh)))     + (fromIntegral(mapy*zoneh)) - (camy)
        nz = -2.0*(fromIntegral (max zonew zoneh)) + (zoom)
        gridratio = (fromIntegral zoneh) / (fromIntegral zonew)

-- draws the zone screen
drawZone :: State -> Env -> IO ()
drawZone state env = resequence_ $ map (drawCachedZones zonew zoneh (stateEmbark state) texs (stateZoneCam state) (stateZoom state)) (stateZone state)
  --drawSingleZone x y zonew zoneh zone texs (stateZoneCam state) (stateZoom state)
  where (x, y)   = (stateCursor state) `tupleSub` (stateEmbark state)
        texs     = envZTex env
        zonew    = settingZoneW settings
        zoneh    = settingZoneH settings
        settings = stateSettings state
        zone     = head (stateZone state)

drawCachedZones :: Int -> Int -> (Int, Int) -> [[GL.TextureObject]] -> (Float, Float, Int) -> Float -> Zone -> IO ()
drawCachedZones zonew zoneh embark texs zonecam zoom z = do
  drawSingleZone x y zonew zoneh z texs zonecam zoom
  where (x, y) = (latlong z) `tupleSub` embark

drawSingleZone :: Int -> Int -> Int -> Int -> Zone -> [[GL.TextureObject]] -> (Float, Float, Int) -> Float -> IO ()
drawSingleZone x y zonew zoneh zone texs (camx, camy, camz) zoom = do
  let z     = zonechunk zone
      zgbs  = gbs z
      zg    = expandZone zonew zoneh $ bsToList zgbs 1
      zcbs  = cbs z
      zc    = expandZone zonew zoneh $ bsToList zcbs 1
      zgzip = zip zg zc
  resequence_ (map (drawZoneRow texs x y zonew zoneh camx camy camz zoom) zgzip)
  glFlush

drawZoneRow :: [[GL.TextureObject]] -> Int -> Int -> Int -> Int -> Float -> Float -> Int -> Float -> (([(Int, Int)], Int), ([(Int, Int)], Int)) -> IO ()
drawZoneRow texs mapx mapy zonew zoneh camx camy camz zoom ((a, y), (b, _)) = resequence_ (map (drawZoneSpot texs mapx mapy zonew zoneh camx camy camz zoom y) newzg)
  where newzg = zip a b

drawZoneSpot :: [[GL.TextureObject]] -> Int -> Int -> Int -> Int -> Float -> Float -> Int -> Float -> Int -> ((Int, Int), (Int, Int)) -> IO ()
drawZoneSpot texs mapx mapy zonew zoneh camx camy camz zoom y ((g, x), (c, _)) = withTextures2D tex $ drawZoneTile tex mapx mapy zonew zoneh camx camy camz zoom x y
  where tex = [((texs !! c) !! g)]

drawZoneTile :: [GL.TextureObject] -> Int -> Int -> Int -> Int -> Float -> Float -> Int -> Float -> Int -> Int -> IO ()
drawZoneTile tex mapx mapy zonew zoneh camx camy camz zoom x y = do
  glLoadIdentity
  glTranslatef nx ny nz
  glColor3f 1.0 1.0 1.0
  drawSquare
  where
    (nx, ny, nz) = zoneZoom mapx mapy camx camy x y zonew zoneh zoom
