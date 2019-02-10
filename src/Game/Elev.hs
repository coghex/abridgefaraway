module Game.Elev where

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Split ( chunksOf )
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import GLUtil.Textures
import qualified Graphics.UI.GLFW as GLFW
import System.Random (next)

import Game.Rand
import Game.State
import Game.Settings
import Game.Map

drawElev :: State -> [GL.TextureObject] -> IO ()
drawElev state texs = do
  let gnew = expandGrid $ stateElev state
  resequence_ (map (drawElevRow texs) gnew)
  glFlush

drawElevRow :: [GL.TextureObject] -> ([(Int, Int)], Int) -> IO ()
drawElevRow texs (a, b) = resequence_ (map (drawElevSpot texs b) a)

drawElevSpot :: [GL.TextureObject] -> Int -> (Int, Int) -> IO ()
drawElevSpot texs y (t, x) = withTextures2D [(texs!!10)] $ drawElevTile texs x y t

drawElevTile :: [GL.TextureObject] -> Int -> Int -> Int -> IO ()
drawElevTile texs x y t = do
  glLoadIdentity
  glTranslatef (1.0 + 2*((fromIntegral x) - ((fromIntegral gridw)/2))) (1.0 + 2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-thiszoom)
  glColor3f elev elev $ elevOcean elev
  drawElevSquare
  where elev = ((fromIntegral t)/peaklevel)
        thiszoom = fromIntegral theZoom

elevOcean :: Float -> Float
elevOcean x
  | x <= (sealevel/(peaklevel)) = 8
  | otherwise = x

drawElevSquare :: IO ()
drawElevSquare = do
  glBegin GL_QUADS
  glTexCoord2f    0    0
  glVertex3f    (-1) (-1)  1
  glTexCoord2f    1    0
  glVertex3f      1  (-1)  1
  glTexCoord2f    1    1
  glVertex3f      1    1   1
  glTexCoord2f    0    1
  glVertex3f    (-1)   1   1
  glEnd

dropEvery :: Int -> [Int] -> [Int]
dropEvery _ [] = []
dropEvery n xs = [1] ++ tail (take (n-1) xs) ++ [1] ++ dropEvery n (drop n xs)

blurAdd :: Int -> Int -> Int -> Int
blurAdd x y r     = (4*x)+((r-1)*y)

blurSpots :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int]
blurSpots elev es en ew ee rs rn rw re = do
  let e0 = zipWith3 (blurAdd) elev es rs
      e1 = zipWith3 (blurAdd) e0   en rn
      e2 = zipWith3 (blurAdd) e1   ew rw
      e3 = zipWith3 (blurAdd) e2   ee re
      dv0 = zipWith (*) (take (gridw*gridh) (repeat (5+(quot salt 2)))) (rs)
      dv1 = zipWith (*) (take (gridw*gridh) (repeat (5+(quot salt 2)))) (rn)
      dv2 = zipWith (*) (take (gridw*gridh) (repeat (5+(quot salt 2)))) (rw)
      dv3 = zipWith (*) (take (gridw*gridh) (repeat (5+(quot salt 2)))) (re)
      dv4 = zipWith (+) dv0 dv1
      dv5 = zipWith (+) dv2 dv3
      dv6 = zipWith (+) dv4 dv5
      dv7 = zipWith (*) dv6 (repeat (erosion))
      r1  = zipWith quot e3 $ dv7
      e0b = zipWith (+) r1 es
      e1b = zipWith (+) e0b en
      e2b = zipWith (+) e1b ew
      e3b = zipWith (+) e2b ee
      dvb = take (gridw*gridh) (repeat (5))
  zipWith quot e3b dvb

blurMap :: State -> [Int] -> Int -> [Int]
blurMap state elev 0 = elev
blurMap state elev n = do
  let rs  = randomList (1,salt) (gridw*gridh) (snd (next ((stateStdGens state) !! 1)))
      rn  = randomList (1,salt) (gridw*gridh) (snd (next ((stateStdGens state) !! 2)))
      rw  = randomList (1,salt) (gridw*gridh) (snd (next ((stateStdGens state) !! 3)))
      re  = randomList (1,salt) (gridw*gridh) (snd (next ((stateStdGens state) !! 4)))
      --es0 = drop gridw elev
      --es1 = es0 ++ (take gridw (repeat 1))
      --en0 = take (gridh*gridw - gridw) elev
      --en1 = (take gridw (repeat 1)) ++ en0
      --ew0 = init (1 : elev)
      --ew1 = dropEvery gridw ew0
      --ee0 = tail $ elev ++ [1]
      --ee1 = dropEvery gridw ee0
      (en1, es1, ee1, ew1) = cardinals elev
      out = blurSpots elev es1 en1 ew1 ee1 rs rn rw re
  blurMap state out (n-1)

elevBlurMap :: State -> [Int] -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
elevBlurMap state grid elev l k j i = do
  let e1 = elevMap state grid elev l k j i
  blurMap state e1 erosion

elevMap :: State -> [Int] -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
elevMap state grid elev []     []     []     _ = elev
elevMap state grid elev _      _      _      0 = elev
elevMap state grid elev (l:ls) (k:ks) (j:js) i = do
  let x = findElev state i (fst l) (snd l) grid elev k j
  elevMap state grid x ls ks js (i-1)

findElev :: State -> Int -> Int -> Int -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
findElev _     _ _ _ _ e []     []     = e
findElev state c x y g e (k:ks) (j:js) = do
  let newelev = expandGrid e
      newgrid = expandGrid g
      elev0   = parMap rpar (elevRow state newgrid c (fst k) (snd k) (fst j) (snd j)) newelev
      elev1   = stripGrid elev0
      elev2   = flattenGrid elev1
  findElev state c x y g elev2 ks js

elevRow :: State -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
elevRow state g c w x y z (t1, t2) = (map (elevTile state g c t2 w x y z) t1, t2)

elevTile :: State -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
elevTile state g c j w x y z (t, i) = ((elevOf dist t i j g), i)
  where
    maxdist = 1000 * ((stateSizes state) !! c)
    dist = distance i j w x y z t

elevOf :: Int -> Int -> Int -> Int -> [([(Int, Int)], Int)] -> Int
elevOf dist t x y g = elevOfSpot dist t typ
  where
    typ = (fst $ (fst (g !! y)) !! x)

elevOfSpot :: Int -> Int -> Int -> Int
elevOfSpot dist t 1 = avgElev t $ normElev dist 1 4
elevOfSpot dist t 2 = avgElev t $ -(normElev dist 40 80)
elevOfSpot dist t 3 = avgElev t $ normElev dist 5 6
elevOfSpot dist t 4 = avgElev t $ normElev dist 5 40
elevOfSpot dist t 5 = avgElev t $ normElev dist 20 100
elevOfSpot dist t 6 = avgElev t $ normElev dist 12 40
elevOfSpot dist t typ = 0

normElev :: Int -> Int -> Int -> Int
normElev 0 min max = quot (min+max) 2
normElev x min max = (quot ((max - min)) ((round peaklevel)))*x + min

normElevCrags :: Int -> Int -> Int -> Int
normElevCrags 0 min max = quot (min+max) 2
normElevCrags x min max = round $ (fromIntegral(max - min))/(1.0/fromIntegral(x)) + (fromIntegral(min))

avgElev :: Int -> Int -> Int
avgElev x y = x + (vigor*y)
--avgElev x y = quot ((vigor*x)+y) (vigor + 1)

getElev :: [Int] -> Int -> Int -> Int
getElev e0 x y = do
  let elev = e0 !! (x+(gridw*y))
  round $ ((fromIntegral(elev)) - sealevel)

formatElev :: [Int] -> (Int, Int) -> String
formatElev e (x, y) = "Elev: " ++ (show (getElev e x y))




