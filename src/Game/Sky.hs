module Game.Sky where

import Data.List
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import GLUtil.Textures
import qualified Graphics.UI.GLFW as GLFW

import Game.Map
import Game.Settings
import Game.Data
import Game.State

--data Sky = Sky { lowtroposphere   :: SkyZone
--               , midtroposphere   :: SkyZone
--               , hightroposphere  :: SkyZone
--               , lowstratosphere  :: SkyZone
--               , highstratosphere :: SkyZone
--               } deriving (Show, Eq)
--
--data SkyZone = SkyZone { stemp :: Float
--                       , bar   :: Float
--                       , hum   :: Float
--                       , svx   :: Float
--                       , svy   :: Float
--                       , svz   :: Float
--                       } deriving (Show, Eq)

drawSky :: State -> [GL.TextureObject] -> IO ()
drawSky state texs = do
  let snew = expandGrid $ stateSkies state
      n    = stateSkyTempZ state
  resequence_ (map (drawSkyRow n texs) snew)
  glFlush

drawSkyRow :: Int -> [GL.TextureObject] -> ([(Sky, Int)], Int) -> IO ()
drawSkyRow n texs (a, b) = resequence_ (map (drawSkySpot n texs b) a)

drawSkySpot :: Int -> [GL.TextureObject] -> Int -> (Sky, Int) -> IO ()
drawSkySpot n texs y (s, x) = withTextures2D [(texs!!10)] $ drawSkyTile n texs x y s

drawSkyTile :: Int -> [GL.TextureObject] -> Int -> Int -> Sky -> IO ()
drawSkyTile n texs x y (Sky lt mt ht ls hs) = do
  glLoadIdentity
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  case n of 1     -> case (lt) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+2)/18.0)-1) (abs (((t+2)-18.0)/18.0)) (1.0-((t+2)/18.0))
            2000  -> case (mt) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+2)/8.0)-1) (abs (((t+2)-8.0)/8.0)) (1.0-((t+2)/8.0))
            8000  -> case (ht) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+2)/8.0)-1) (abs (((t+2)-8.0)/8.0)) (1.0-((t+2)/8.0))
            16000 -> case (ht) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+2)/8.0)-1) (abs (((t+2)-8.0)/8.0)) (1.0-((t+2)/8.0))
            24000 -> case (ht) of Land t              -> glColor3f 1.0 1.0 1.0
                                  SkyZone t _ _ _ _ _ -> glColor3f (((t+2)/8.0)-1) (abs (((t+2)-8.0)/8.0)) (1.0-((t+2)/8.0))
  drawSkySquare
  where thiszoom = fromIntegral theZoom

drawSkySquare :: IO ()
drawSkySquare = do
  glBegin GL_QUADS
  glTexCoord2f   0    1
  glVertex3f   (-1) (-1)  1
  glTexCoord2f   1    1
  glVertex3f     1  (-1)  1
  glTexCoord2f   1    0
  glVertex3f     1    1   1
  glTexCoord2f   0    0
  glVertex3f   (-1)   1   1
  glEnd

formatSkyTemp :: Int -> [Sky] -> (Int, Int) -> String
formatSkyTemp n ss (x, y) = getSkyTemp n s x y
  where s = (ss !! (x+gridw*y))

getSkyTemp :: Int -> Sky -> Int -> Int -> String
--getSkyTemp n     (Land t)             x y = "Solid Earth Temp: "       ++ (show t)
getSkyTemp 1     (Sky lt mt ht ls hs) x y = "Low Troposphere Temp: "   ++ (getSkyTempMaybe lt)
getSkyTemp 2000  (Sky lt mt ht ls hs) x y = "Mid Troposphere Temp: "   ++ (getSkyTempMaybe mt)
getSkyTemp 8000  (Sky lt mt ht ls hs) x y = "High Troposphere Temp: "  ++ (getSkyTempMaybe ht)
getSkyTemp 16000 (Sky lt mt ht ls hs) x y = "Low Stratosphere Temp: "  ++ (getSkyTempMaybe ls)
getSkyTemp 24000 (Sky lt mt ht ls hs) x y = "High Stratosphere Temp: " ++ (getSkyTempMaybe hs)

getSkyTempMaybe :: SkyZone -> String
getSkyTempMaybe s = case (s) of SkyZone t _ _ _ _ _ -> showFloatFoReal $ roundTo precision t
                                Land    t           -> "Land Temp: " ++ (showFloatFoReal $ roundTo precision t)

theExpanseAbove :: [Ocean] -> [Int] -> [Int] -> [Float] -> [Sky]
theExpanseAbove ocean grid elev light = newSkies ocean grid elev light

newSkies :: [Ocean] -> [Int] -> [Int] -> [Float] -> [Sky]
newSkies o g e l = zipWith4 newSky o g e l

newSky :: Ocean -> Int -> Int -> Float -> Sky
newSky o g e l = Sky { lowtroposphere   = newZone o g e l 1     0 0 0
                     , midtroposphere   = newZone o g e l 2000  0 0 0
                     , hightroposphere  = newZone o g e l 8000  0 0 0
                     , lowstratosphere  = newZone o g e l 16000 0 0 0
                     , highstratosphere = newZone o g e l 24000 0 0 0
                     }

newZone :: Ocean -> Int -> Int -> Float -> Int -> Float -> Float -> Float -> SkyZone
newZone o g e l n newvx newvy newvz = SkyZone { stemp = initSkyTemp o
                                              , bar   = initSkyPres
                                              , hum   = initSkyHum o
                                              , svx   = newvx
                                              , svy   = newvy
                                              , svz   = newvz
                                              }

initSkyTemp :: Ocean -> Float
initSkyTemp (Dry _) = 1.0
initSkyTemp s       = (temp (epipelagic s))

initSkyPres :: Float
initSkyPres = 1.0

initSkyHum :: Ocean -> Float
initSkyHum (Dry _) = 1.0
initSkyHum s       = 2.0

decreaseSkyZ :: Int -> Int
decreaseSkyZ 1     = 1
decreaseSkyZ 2000  = 1
decreaseSkyZ 8000  = 2000
decreaseSkyZ 16000 = 8000
decreaseSkyZ 24000 = 16000
decreaseSkyZ x     = x

increaseSkyZ :: Int -> Int
increaseSkyZ 1     = 2000
increaseSkyZ 2000  = 8000
increaseSkyZ 8000  = 16000
increaseSkyZ 16000 = 24000
increaseSkyZ 24000 = 24000
increaseSkyZ x     = x



