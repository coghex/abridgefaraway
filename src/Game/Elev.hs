module Game.Elev where

import Data.List.Split ( chunksOf )
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW

import Game.Rand
import Game.State
import Game.Settings
import Game.Map

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

drawElev :: State -> IO ()
drawElev state = do
  let gnew = expandGrid $ stateElev state
  resequence_ (map (drawElevRow) gnew)
  glFlush

drawElevRow :: ([(Int, Int)], Int) -> IO ()
drawElevRow (a, b) = resequence_ (map (drawElevSpot b) a)

drawElevSpot :: Int -> (Int, Int) -> IO ()
drawElevSpot y (t, x) = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-500)
  glColor3f elev elev elev
  drawElevSquare
  where elev = (fromIntegral t)/1000

drawElevSquare :: IO ()
drawElevSquare = do
  glBegin GL_QUADS
  glVertex3f    (-1) (-1)  1
  glVertex3f      1  (-1)  1
  glVertex3f      1    1   1
  glVertex3f    (-1)   1   1
  glEnd

blurMap :: Env -> [Int] -> [Int]
blurMap env elev = do
  let r1 = randomList (-fudge,fudge)
  elev

elevBlurMap :: State -> Env -> [Int] -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
elevBlurMap state env grid elev l k j i = do
  let e1 = blurMap env elev
  elevMap state grid e1 l k j i

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
      elev0   = map (elevRow state newgrid c (fst k) (snd k) (fst j) (snd j)) newelev
      elev1   = stripGrid elev0
      elev2   = flattenGrid elev1
  findElev state c x y g elev2 ks js

elevRow :: State -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
elevRow state g c w x y z (t1, t2) = (map (elevTile state g c t2 w x y z) t1, t2)

elevTile :: State -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
elevTile state g c j w x y z (t, i) = ((elevOf t i j g), i)
  where
    maxdist = 1000 * ((stateSizes state) !! c)
    dist = distance i j w x y z t

elevOf :: Int -> Int -> Int -> [([(Int, Int)], Int)] -> Int
elevOf t x y g = elevOfSpot t typ
  where
    typ = (fst $ (fst (g !! y)) !! x)

elevOfSpot :: Int -> Int -> Int
elevOfSpot t 1 = 1
elevOfSpot t 3 = 100
elevOfSpot t 4 = 200
elevOfSpot t 5 = 500
elevOfSpot t 6 = 300
elevOfSpot t typ = round $ (fromIntegral (t + typ)) / 2

