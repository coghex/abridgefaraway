module World where

import System.Random
import Data.List.Split
import Data.Bits ( (.|.) )
import Control.Monad
import Control.Monad.RWS.Strict ( get )
import Graphics.GL
import Graphics.GLU
import qualified Graphics.UI.GLFW as GLFW
import State

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

sceneSetup :: IO ()
sceneSetup = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT

workRows :: [Int] -> [(Int, Int)]
workRows l = do
  zip l [0..120]

expandMap :: [Int] -> [([(Int, Int)], Int)]
expandMap m = zip (map workRows (chunksOf 120 m)) [0..90]

flattenMap :: [[Int]] -> [Int]
flattenMap xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

stripRow :: [(Int, Int)] -> [Int]
stripRow ((a,b):ys) = a : stripRow ys
stripRow _          = []

stripMap :: [([(Int, Int)], Int)] -> [[Int]]
stripMap ((a,b):ys) = (stripRow a) : stripMap ys
stripMap _          = [[]]

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance tile a b x1 y1 x2 y2 = do
  let t = tile+1
  let p1 = ((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1))
  let p2 = ((x2-a)*(x2-a))+((y2-b)*(y2-b))
  p1*p2*t

drawWorld :: [GLuint] -> [([(Int, Int)], Int)] -> [IO()]
drawWorld texs m = map (drawRow texs) m

drawRow :: [GLuint] -> ([(Int, Int)], Int) -> IO ()
drawRow texs (a, b) = resequence_ (map (drawSpot texs b) a)

drawSpot :: [GLuint] -> Int -> (Int, Int) -> IO ()
drawSpot texs y (t,x) = (drawTile texs x y t)

drawTile :: [GLuint] -> Int -> Int -> Int -> IO ()
drawTile texs x y t = do
  glLoadIdentity
  glTranslatef ((fromIntegral x)-60.0) ((fromIntegral y)-45.0) (-200)

  glBindTexture GL_TEXTURE_2D (texs!!t)
  glBegin GL_QUADS
  glTexCoord2f   0    0
  glVertex3f   (-1) (-1)    1  -- bottom left of quad (Front)
  glTexCoord2f   1    0
  glVertex3f     1  (-1)    1  -- bottom right of quad (Front)
  glTexCoord2f   1    1 
  glVertex3f     1    1     1  -- top right of quad (Front)
  glTexCoord2f   0    1 
  glVertex3f   (-1)   1     1  -- top left of quad (Front)
  glTexCoord2f   1    0 
  glVertex3f   (-1) (-1) (-1)  -- bottom right of quad (Back)
  glTexCoord2f   1    1 
  glVertex3f   (-1)   1  (-1)  -- top right of quad (Back)
  glTexCoord2f   0    1 
  glVertex3f     1    1  (-1)  -- top left of quad (Back)
  glTexCoord2f   0    0 
  glVertex3f     1  (-1) (-1)  -- bottom left of quad (Back)
  
  glEnd

drawScene :: State -> GLFW.Window -> IO ()
drawScene state win = do
 -- state <- get
  let mapnew = expandMap (stateGrid state)
  resequence_ (drawWorld (stateTexs state) mapnew)
  glFlush

buildList :: ([a], [a], [a], [a], [a], [a], [a]) -> [(a, a, a, a, a, a, a)]
buildList ([], [], [], [], [], [], []) = []
buildList (a:as, b:bs, c:cs, d:ds, e:es, f:fs, g:gs) = [(a, b, c, d, e, f, g)] ++ buildList (as, bs, cs, ds, es, fs, gs)

seedTile :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedTile _ _ _    _    _    _    _    0  (a, i) = (7, i)
seedTile _ _ _    _    _    _    _    1  (a, i) = (7, i)
seedTile _ _ _    _    _    _    _    2  (a, i) = (7, i)
seedTile _ _ _    _    _    _    _    88 (a, i) = (7, i)
seedTile _ _ _    _    _    _    _    89 (a, i) = (7, i)
seedTile x y xsiz ysiz xran yran tile j  m
  | (distance tile xran yran x y j (snd m) <= (100*xsiz*ysiz)) = (tile, (snd m))
  | otherwise                                           = ((fst m), (snd m))

seedRow :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow x y xsiz ysiz xran yran tile m = do
  (map (seedTile x y xsiz ysiz xran yran tile (snd m)) (fst m), (snd m))

seedWorld :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [([(Int, Int)], Int)] -> [([(Int, Int)], Int)]
seedWorld _ _ _    _    _    _    4    m = m
seedWorld _ _ _    _    _    _    6    m = m
seedWorld _ _ _    _    _    _    7    m = m
seedWorld x y xsiz ysiz xran yran tile m = do
  map (seedRow x y xsiz ysiz xran yran tile) m

buildWorld :: [([(Int, Int)], Int)] -> [(Int, Int, Int, Int, Int, Int, Int)] -> [([(Int, Int)], Int)]
buildWorld m []           = m
buildWorld m ((x, y, xsiz, ysiz, xran, yran, tile):xs) = do
  buildWorld (seedWorld x y xsiz ysiz xran yran tile m) xs

seedMap :: [GLuint] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int]
seedMap texs l xs ys xsiz ysiz xran yran tile = do
  let x = buildList (xs, ys, xsiz, ysiz, xran, yran, tile)
  let mapexp = expandMap l
  let map2 = buildWorld mapexp x
  let map3 = stripMap map2
  let map4 = flattenMap map3
  map4

buildMap :: State -> State
buildMap state = do
  let texs = stateTexs state
  let grid = stateGrid state
  let xs = stateXs state
  let ys = stateYs state
  let xsiz = stateXSizes state
  let ysiz = stateYSizes state
  let xran = stateXRands state
  let yran = stateYRands state
  let ssed = stateSeeds state
  let ixr = stateIceXRands state
  let iyr = stateIceYRands state
  let l = seedMap texs grid xs ys xsiz ysiz xran yran ssed
  let l2 = iceMap l xran yran xsiz ysiz ixr iyr
  State
    { stateGrid      = l2
    , stateTexs      = texs
    , stateGame      = SWorld
    , stateXs        = xs
    , stateYs        = ys
    , stateXSizes    = xsiz
    , stateYSizes    = ysiz
    , stateXRands    = xran
    , stateYRands    = yran
    , stateSeeds     = ssed
    , stateIceXRands = ixr
    , stateIceYRands = iyr
    }

iceMap :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int]
iceMap l xran yran xsiz ysiz ixr iyr = do
  let mapexp = chunksOf 120 l
  let mapnew = zip (map workRows mapexp) [0..90]
  --let x = buildList (ixr, iyr, xsiz, ysiz, xran, yran, (take 6 (repeat (length ixr))))
  --let map0 = buildWorld mapnew x
  let map1 = stripMap mapnew
  let map2 = flattenMap map1
  map2

randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
randomList bnds n = take n . randomRs bnds

randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR(min, max)

changeSpot :: Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
changeSpot x y j t (a, i) | ((x==i) && (y==j)) = (t, i)
                          | otherwise          = (a, i)

changeRow :: Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
changeRow x y t m = ((map (changeSpot x y (snd m) t) (fst m)), (snd m))

changeTile :: [Int] -> (Int, Int) -> Int -> [Int]
changeTile m (x, y) t = do
  let mapexp = chunksOf 120 m
  let mapnew = zip (map workRows mapexp) [0..90]
  let map0 = map (changeRow x y t) mapnew
  let map1 = stripMap map0
  let map2 = flattenMap map1
  map2
