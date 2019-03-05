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
  , stateZoom           = stateZoom state
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
  , stateSkyTempZ       = stateSkyTempZ state
  , stateWindZ          = stateWindZ state
  , stateRainZ          = stateRainZ state
  , stateVolcanism      = stateVolcanism state
  , stateZones          = stateZones state
  , stateUnits          = units }

theTex :: [GL.TextureObject] -> Int -> [GL.TextureObject]
theTex texs 0 = texs
theTex texs n = theTex newtex (n-1)
  where newtex = ((tail texs) ++ [(head texs)])

evalUnits :: State -> Env -> [Unit]
evalUnits state env = map (evalUnit utexs) units
  where units = stateUnits state
        utexs = envUnitTex env

evalUnit :: [[GL.TextureObject]] -> Unit -> Unit
evalUnit utexs u0 = Unit { unittexs = evalAction utexs texs up ud uf uac 
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

evalAction :: [[GL.TextureObject]] -> [GL.TextureObject] -> (Float, Float) -> Int -> Int -> Action -> [GL.TextureObject]
evalAction _     texs _   dir frame NullAction          = theTex (texs) frame
evalAction utexs _    pos dir frame (MoveTo dest speed) = theTex (utexs !! (moveDirection pos dest dir)) frame
evalAction utexs _    pos dir frame (Idle p)            = theTex (utexs !! dir) frame
evalAction _     texs _   dir frame _                   = theTex (texs) frame

nextAction :: (Float, Float) -> Action -> Action
nextAction _   NullAction          = NullAction
nextAction pos (MoveTo dest speed)
  | dist <= 0.1 = Idle { pos = pos }
  | otherwise    = MoveTo { dest  = dest
                          , speed = speed }
  where dist = zoneLinearDistance pos dest
nextAction _   a                   = a

zoneAction :: Action -> (Int, Int) -> (Int, Int)
zoneAction ac zone = zone

posAction :: Action -> (Float, Float) -> (Float, Float)
posAction NullAction              pos      = pos
posAction (Idle pos)              _        = pos
posAction (MoveTo (dx, dy) speed) (sx, sy) = (sx+rx, sy+ry)
  where rx   = speed*((x/dist)/32.0)
        ry   = speed*((y/dist)/32.0)
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
      zoom = stateZoom state
  withTextures2D [(head texs)] $ drawUnitTile [(head texs)] camx camy zoom unit

animateJustUnits :: [Unit] -> [Unit]
animateJustUnits units = map animateUnit units

animateUnits :: State -> [Unit]
animateUnits state = map animateUnit units
  where units = stateUnits state

animateUnit :: Unit -> Unit
animateUnit u0 = Unit { unittexs = texs--frameControl len texs uf
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
        len  = length texs

--frameControl :: Int -> [GL.TextureObject] -> Int -> [GL.TextureObject]
--frameControl len texs frame
--  | frame >= 0 = ((tail texs) ++ [(head texs)])
--  | otherwise    = texs

frameCounter :: Int -> Int -> Int
frameCounter len frame
  | frame >= len = 0
  | otherwise    = frame+1

drawUnitTile :: [GL.TextureObject] -> Float -> Float -> Float -> Unit -> IO ()
drawUnitTile tex camx camy zoom unit = do
  glLoadIdentity
  glTranslatef (2*((tx) - ((fromIntegral zonew)/2))) (2*((ty) - ((fromIntegral zoneh)/2))) (-zoom/4)
  glColor3f 1.0 1.0 1.0
  drawSquare
  where (x, y)   = position unit
        tx       = x + camx
        ty       = y + camy
        thiszoom = fromIntegral theZoom
