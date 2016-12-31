module Game.Zone where

import Data.List.Split (chunksOf)
import System.Random
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import Game.Settings
import Game.State
import Game.Draw
import Game.Rand
import Game.World

setZone :: [Int] -> Int -> Int -> [Int]
setZone a x y = do
  let zmap = zip a [0..(gridh*gridw)]
  fst $ unzip $ map (zoneMapper (x + (gridw*y))) zmap

zoneMapper :: Int -> (Int, Int) -> (Int, Int)
zoneMapper a (b, c)
  | a==c      = (1, c)
  | otherwise = (b, c)

bushMapper :: State -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int] -> [([(Int, Int)], Int)] -> [([(Int, Int)], Int)]
bushMapper _     _ []         []           []        z = z
bushMapper state r ((x,y):xs) ((x2,y2):ys) ((x3):zs) z = do
  let zn = map (makeZoneBush state r x y x2 y2 x3) z
  bushMapper state r xs ys zs zn

makeZoneBush :: State -> [Int] -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
makeZoneBush state r x y x2 y2 s (t, j) = ((map (makeZoneBushSpot state r j x y x2 y2 s) t), j)

bushQuadrant :: Int -> Int -> Int -> Int -> Int
bushQuadrant x1 y1 x2 y2 
  | ((x1-x2)>=0) && ((y1-y2)>=0) = if ((x1-x2) - (y1-y2))>1 then 6 else if ((x1-x2) - (y1-y2))<(-1) then 2 else 3
  | ((x1-x2)>=0) && ((y1-y2)<0)  = if ((x1-x2) + (y1-y2))>1 then 8 else if ((x1-x2) + (y1-y2))<(-1) then 6 else 9
  | ((x1-x2)<0) && ((y1-y2)>=0)  = if ((y1-y2) + (x1-x2))>1 then 2 else if ((y1-y2) + (x1-x2))<(-1) then 4 else 1
  | ((x1-x2)<0) && ((y1-y2)<0)   = if (((x1-x2)) + (y1-y2))>1 then 4 else if (((x1-x2)) + (y1-y2))<(-1) then 8 else 4
  | otherwise                    = 5

makeZoneBushSpot :: State -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
makeZoneBushSpot state r j x y x2 y2 s (t, i)
  | (i>=zonew)||(j>=zoneh)                              = (t,i)
  | (distance (i) (j) (x) (y) x2 y2 1) < 1000*s = (5, i)
  | ((distance (i) (j) (x) (y) x2 y2 1) < 1400*s) && ((distance (i) (j) (x) (y) x2 y2 1) >= 1000*s) = ((bushQuadrant i j x y), i)
  | otherwise                                           = (t, i)

initZone :: State -> [Int] -> Int -> [Int]
initZone state r t = do
  let x = (take (zonew*zoneh) (repeat t))
  let znew = expandZone x
  let z0 = map (makeZone state r) znew
  --let z1 = map (makeZoneBush state r 0 0) z0
  let z1 = bushMapper state r (stateBushes state) (stateBRands state) (stateBSizes state) z0
  let z2 = stripZone z1
  flattenZone z2

makeZone :: State -> [Int] -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
makeZone state r (m, j) = ((map (makeZSpot r j state) m),j)

makeZSpot :: [Int] -> Int -> State -> (Int, Int) -> (Int, Int)
makeZSpot r j state (l, i) = ((seedZSpot state r i j l), i)

seedZSpot :: State -> [Int] -> Int -> Int -> Int -> Int
seedZSpot state r i j l = r!!(i+(j*zonew))

drawZone :: State -> GL.GLfloat -> GL.GLfloat -> [[GL.TextureObject]] -> IO ()
drawZone state camx camy texs = do
  let x = fst (stateCursor state) 
  let y = snd (stateCursor state)
  let znew = expandZone $ stateCurrentZ state
  resequence_ (map (drawZoneRow texs camx camy (getZoneType state x y)) znew)
  drawPaths texs (round camx) (round camy) (statePaths state) (getZoneType state x y)
  GL.flush
--let x = fst (stateCursor state) 
  --let y = snd (stateCursor state)
  --let t = getZoneType state x y
  --withTextures2D [((texs!!1)!!10)] $ drawZoneTile (texs!!1) 120 90 10
  --drawZoneSpot texs 0 0 10 t


drawPaths :: [[GL.TextureObject]] -> Int -> Int -> [Int] -> Int -> IO ()
drawPaths _    _    _    _ 4 = return ()
drawPaths _    _    _    _ 5 = return ()
drawPaths texs camx camy z t = do
  let z0 = expandZone z
  resequence_ (map (drawPathRow texs camx camy) z0)

drawPathRow :: [[GL.TextureObject]] -> Int -> Int -> ([(Int, Int)], Int) -> IO ()
drawPathRow texs camx camy (a, b) = resequence_ (map (drawPathSpot texs camx camy b) a)

drawPathSpot :: [[GL.TextureObject]] -> Int -> Int -> Int -> (Int, Int) -> IO ()
drawPathSpot _    _    _    _ (0, i) = return ()
drawPathSpot texs camx camy j (t, i) = withTextures2D [((texs!!4)!!t)] $ drawZoneTile (texs!!4) (i+camx) (j+camy) 0 50

drawZoneSpot :: [[GL.TextureObject]] -> GL.GLfloat -> GL.GLfloat -> Int -> Int -> (Int, Int) -> IO ()
drawZoneSpot texs camx camy c y (t, x)
  | (c==4 || c==5)       = withTextures2D [((texs!!(c+1))!!0)] $ drawZoneTile (texs!!c) (x+(round camx)) (y+(round camy)) t 50
  | (c >= 0) && (c < 3)  = withTextures2D [((texs!!c)!!t)] $ drawZoneTile (texs!!c) (x+(round camx)) (y+(round camy)) t 50
  | otherwise            = print "no tex"

getZoneType :: State -> Int -> Int -> Int
getZoneType state x y = (stateGrid state)!!(x+(gridw*y))

expandZone :: [Int] -> [([(Int, Int)], Int)]
expandZone m = zip (map workZRows (chunksOf zonew m)) [0..zoneh]

workZRows :: [Int] -> [(Int, Int)]
workZRows l = do
  zip l [0..zonew]

stripZone :: [([(Int, Int)], Int)] -> [[Int]]
stripZone ((a,b):ys) = (stripZRow a) : stripZone ys
stripZone _          = [[]]

stripZRow :: [(Int, Int)] -> [Int]
stripZRow ((a, b):ys) = a : stripZRow ys
stripZRow _           = []

flattenZone :: [[Int]] -> [Int]
flattenZone xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

drawZoneRow :: [[GL.TextureObject]] -> GL.GLfloat -> GL.GLfloat -> Int -> ([(Int, Int)], Int) -> IO ()
drawZoneRow texs camx camy c (a,b) = resequence_ (map (drawZoneSpot texs camx camy c b) a)
