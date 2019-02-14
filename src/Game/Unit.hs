module Game.Unit where

import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import System.Random (StdGen, mkStdGen)
import qualified Graphics.UI.GLFW as GLFW
import GLUtil.Textures
import Data.Time.Clock
import Control.Concurrent

import Game.Settings
import Game.State
import Game.Data
import Game.Draw
import Game.Map
import Game.Zone

unitState :: State -> [Unit] -> State
unitState state units = State
  { stateGame           = stateGame state
  , stateStdGens        = stateStdGens state
  , stateScreenW        = stateScreenW state
  , stateScreenH        = stateScreenH state
  , stateGrid           = stateGrid state
  , stateOG             = stateOG state
  , stateElev           = stateElev state
  , stateCursor         = stateCursor state
  , stateNConts         = stateNConts state
  , stateCurrMap        = stateCurrMap state
  , stateConts          = stateConts state
  , stateSeeds          = stateSeeds state
  , stateRands          = stateRands state
  , stateSizes          = stateSizes state
  , stateTypes          = stateTypes state
  , stateRandI          = stateRandI state
  , stateRangeRands     = stateRangeRands state
  , stateSun            = stateSun state
  , stateMoon           = stateMoon state
  , stateSunSpots       = stateSunSpots state
  , stateTime           = stateTime state
  , stateOceans         = stateOceans state
  , stateOceanTempZ     = stateOceanTempZ state
  , stateOceanCurrentsZ = stateOceanCurrentsZ state
  , stateSkies          = stateSkies state
  , stateZones          = stateZones state
  , stateUnits          = units }

evalUnits :: State -> Env -> [Unit]
evalUnits state env = map (evalUnit utexs) units
  where units = stateUnits state
        utexs = envUnitTex env

evalUnit :: [[GL.TextureObject]] -> Unit -> Unit
evalUnit utexs u0 = Unit { unittexs = evalAction utexs texs up ud uac 
                         , frame    = uf
                         , unittype = ut
                         , action   = nextAction up uac
                         , zone     = zoneAction uac uz
                         , position = posAction uac up
                         , dir      = dirAction uac up ud }
  where texs = unittexs u0
        uf   = frame u0
        ut   = unittype u0
        uz   = zone u0
        up   = position u0
        uac  = action u0
        ud   = dir u0

evalAction :: [[GL.TextureObject]] -> [GL.TextureObject] -> (Float, Float) -> Int -> Action -> [GL.TextureObject]
evalAction _     texs _   dir NullAction          = texs
evalAction utexs _    pos dir (MoveTo dest speed) = utexs !! (moveDirection pos dest dir)
evalAction _     texs _   dir _                   = texs

nextAction :: (Float, Float) -> Action -> Action
nextAction _   NullAction          = NullAction
nextAction pos (MoveTo dest speed)
  | dist <= 0.01 = NullAction
  | otherwise    = MoveTo { dest  = dest
                          , speed = speed }
  where dist = zoneLinearDistance pos dest
nextAction _   _                   = NullAction

zoneAction :: Action -> (Int, Int) -> (Int, Int)
zoneAction ac zone = zone

posAction :: Action -> (Float, Float) -> (Float, Float)
posAction NullAction              pos      = pos
posAction (MoveTo (dx, dy) speed) (sx, sy) = (sx+rx, sy+ry)
  where rx   = sf*((x/dist)/32.0)
        ry   = sf*((y/dist)/32.0)
        sf   = fromIntegral speed
        x    = dx-sx
        y    = dy-sy
        dist = zoneLinearDistance (sx, sy) (dx, dy)
posAction _ pos = pos

dirAction :: Action -> (Float, Float) -> Int -> Int
dirAction NullAction _ _              = 0
dirAction (MoveTo dest speed) pos dir = moveDirection pos dest dir
dirAction _                   _   dir = dir

drawUnits :: State -> IO ()
drawUnits state = resequence_ (map (drawUnit state) units)
  where units = stateUnits state

drawUnit :: State -> Unit -> IO ()
drawUnit state unit = do
  let texs = unittexs unit
      (camx, camy, _) = getZoneCam (head (stateZones state))
  withTextures2D [(head texs)] $ drawUnitTile [(head texs)] camx camy unit

animateJustUnits :: [Unit] -> [Unit]
animateJustUnits units = map animateUnit units

animateUnits :: State -> [Unit]
animateUnits state = map animateUnit units
  where units = stateUnits state

animateUnit :: Unit -> Unit
animateUnit u0 = Unit { unittexs = frameControl len texs uf
                      , frame    = frameCounter len uf
                      , unittype = ut
                      , action   = uac
                      , zone     = uz
                      , position = up
                      , dir      = ud }
  where texs = unittexs u0
        uf   = frame u0
        ut   = unittype u0
        uz   = zone u0
        up   = position u0
        uac  = action u0
        ud   = dir u0
        len  = 0

frameControl :: Int -> [GL.TextureObject] -> Int -> [GL.TextureObject]
frameControl len texs frame
  | frame >= len = ((tail texs) ++ [(head texs)])
  | otherwise    = texs

frameCounter :: Int -> Int -> Int
frameCounter len frame
  | frame >= len = 0
  | otherwise    = frame+1

drawUnitTile :: [GL.TextureObject] -> Float -> Float -> Unit -> IO ()
drawUnitTile tex camx camy unit = do
  glLoadIdentity
  glTranslatef (2*((tx) - ((fromIntegral zonew)/2))) (2*((ty) - ((fromIntegral zoneh)/2))) (-zoom/4)
  glColor3f 1.0 1.0 1.0
  drawSquare
  where (x, y)   = position unit
        tx       = x + camx
        ty       = y + camy
        thiszoom = fromIntegral theZoom
