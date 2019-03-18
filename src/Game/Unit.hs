module Game.Unit where

import Graphics.GL
import Data.List (elemIndex)
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
  , stateDesireability  = stateDesireability state
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
                         , actions  = nextAction (actions u0) up
                         , zone     = zoneAction uac uz
                         , position = posAction uac up
                         , dir      = dirAction uac up ud }
  where texs = unittexs u0
        uf   = frame u0
        ut   = unittype u0
        uz   = zone u0
        up   = position u0
        uac  = head $ actions u0
        ud   = dir u0

evalAction :: [[GL.TextureObject]] -> [GL.TextureObject] -> (Float, Float) -> Int -> Int -> Action -> [GL.TextureObject]
evalAction _     texs _   dir frame NullAction          = theTex (texs) frame
evalAction utexs _    pos dir frame (MoveTo dest speed) = theTex (utexs !! (moveDirection pos dest dir)) frame
evalAction utexs _    pos dir frame (Idle p)            = theTex (utexs !! dir) frame
evalAction _     texs _   dir frame _                   = theTex (texs) frame

nextAction :: [Action] -> (Float, Float) -> [Action]
nextAction [] ( _,  _) = [NullAction]
nextAction ac (px, py)
  | actionComplete (head ac) (px, py) = tail ac
  | otherwise                         = ac

actionComplete :: Action -> (Float, Float) -> Bool
actionComplete (NullAction)        ( _,  _) = True
actionComplete (Idle _)            ( _,  _) = False
actionComplete (MoveTo dest speed) (px, py)
  | dist <= 0.1 = True
  | otherwise   = False
  where dist     = zoneLinearDistance (px, py) (dx, dy)
        (dx, dy) = dest

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

moveAtSpeed :: (Float, Float) -> (Float, Float) -> Float -> [Action] -> [Action]
moveAtSpeed (px, py) (dx, dy) s a
  | dist <= 0.2 = [Idle { pos = (px, py) }]
  | otherwise   = nextm:(moveAtSpeed (dest nextm) (dx, dy) s a)
  where dist  = zoneLinearDistance (px, py) (dx, dy)
        nextm = (moveOne (px, py) (dx, dy) s)

moveOne :: (Float, Float) -> (Float, Float) -> Float -> Action
moveOne pos dest s = MoveTo { dest   = (x, y)
                            , speed  = s
                            }
  where (x, y) = findMove pos dest

findMove :: (Float, Float) -> (Float, Float) -> (Float, Float)
findMove (px, py) dest = (x, y)
  where (x, y) = bestUnitMove (px, py) bs
        bs     = elemIndex (maximum scores) scores
        scores = [n, s, e, w, ne, nw, se, sw]
        n      = scoreMove (           px, (py+pathstep)) (px, py) dest
        s      = scoreMove (           px, (py-pathstep)) (px, py) dest
        e      = scoreMove ((px+pathstep),            py) (px, py) dest
        w      = scoreMove ((px-pathstep),            py) (px, py) dest
        ne     = scoreMove ((px+pathstep), (py+pathstep)) (px, py) dest
        nw     = scoreMove ((px-pathstep), (py+pathstep)) (px, py) dest
        se     = scoreMove ((px+pathstep), (py-pathstep)) (px, py) dest
        sw     = scoreMove ((px-pathstep), (py-pathstep)) (px, py) dest

bestUnitMove :: (Float, Float) -> Maybe Int -> (Float, Float)
bestUnitMove (px, py) Nothing  = (px, py)
bestUnitMove (px, py) (Just 1) = (           px, (py+pathstep))
bestUnitMove (px, py) (Just 2) = (           px, (py-pathstep))
bestUnitMove (px, py) (Just 3) = ((px+pathstep),            py)
bestUnitMove (px, py) (Just 4) = ((px-pathstep),            py)
bestUnitMove (px, py) (Just 5) = ((px+pathstep), (py+pathstep))
bestUnitMove (px, py) (Just 6) = ((px-pathstep), (py+pathstep))
bestUnitMove (px, py) (Just 7) = ((px+pathstep), (py-pathstep))
bestUnitMove (px, py) (Just 8) = ((px-pathstep), (py-pathstep))


scoreMove :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float
scoreMove move pos dest = score
  where score = (dist - distm)
        dist  = zoneLinearDistance pos dest
        distm = zoneLinearDistance move dest

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
                      , actions  = uac
                      , zone     = uz
                      , position = up
                      , dir      = ud }
  where texs = unittexs u0
        uf   = frame u0
        ut   = unittype u0
        uz   = zone u0
        up   = position u0
        uac  = actions u0
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
