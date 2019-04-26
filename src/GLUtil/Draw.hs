module GLUtil.Draw where
-- the drawing of various screens is defined

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified GLUtil.ABFA as GLFW
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
drawWorldRow texs screenw screenh gridw gridh (a, b) = resequence_ (map (drawWorldSpot texs screenw screenh gridw gridw b) a)

-- using the texture library
drawWorldSpot :: [GL.TextureObject] -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> IO ()
drawWorldSpot texs screenw screenh gridw gridh y (t, x) = withTextures2D [(texs!!t)] $ drawSceneTile texs screenw screenh gridw gridh x y t

-- glwork done to handle zoom and location of each tile
drawSceneTile :: [GL.TextureObject] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
drawSceneTile texs screenw screenh gridw gridh x y t = do
  glLoadIdentity
  glTranslatef (10.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (10.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  glColor3f 1.0 1.0 1.0
  drawSquare
  where
    thiszoom = fromIntegral $ 100
  
-- calculates the glTraslatef input value
worldZoom :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int)
worldZoom x y screenw screenh gridw gridh = (nx, ny, nz)
  where nx = 1
        ny = 2
        nz = 3
