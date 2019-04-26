module ABFA.Elev where
-- functions to calculate the elevation of the world map

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Split (chunksOf)

import ABFA.Rand
import ABFA.State
import ABFA.Data
import ABFA.Map

-- makes sure if elevation < sealevel the tile is underwater
elevOcean :: Float -> Float -> Float -> Float
elevOcean sealevel peaklevel x
  | x <= (sealevel/(peaklevel)) = 8
  | otherwise                   = x

-- calculate elevation and apply noise
elevBlurMap :: State -> [Int] -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
elevBlurMap state grid elev l k j i = do
  let e1 = elevMap state grid elev l k j i
  blurMap state e1 erosion
  where erosion = 2

-- creates the initial elevation
elevMap :: State -> [Int] -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
elevMap state grid elev []     []     []     _ = elev
elevMap state grid elev _      _      _      0 = elev
elevMap state grid elev (l:ls) (k:ks) (j:js) i = do
  let x = findElev state i (fst l) (snd l) grid elev k j
  elevMap state grid x ls ks js (i-1)

-- finds elevation for each continent
findElev :: State -> Int -> Int -> Int -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
findElev _     _ _ _ _ e []     []     = e
findElev state c x y g e (k:ks) (j:js) = do
  let newelev  = expandGrid gridw gridh e
      newgrid  = expandGrid gridw gridh g
      gridw    = settingGridW settings
      gridh    = settingGridH settings
      settings = stateSettings state
      elev0    = parMap rpar (elevRow state newgrid c (fst k) (snd k) (fst j) (snd j)) newelev
      elev1    = stripGrid elev0
      elev2    = flattenGrid elev1
  findElev state c x y g elev2 ks js

-- find elevation of a row of a continent
elevRow :: State -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
elevRow state g c w x y z (t, i) = (t, i)
  where
    maxdist = 1000 * ((wpSizes wparams) !! c)
    wparams = stateWParams state

-- elevation of a single spot
elevOf :: Int -> Float -> Int -> Int -> Int -> Int -> [([(Int, Int)], Int)] -> Int
elevOf v pl dist t x y g = elevOfSpot v pl dist t typ
  where
    typ = (fst $ (fst (g !! y)) !! x)

-- these are the upper and lower bounds (relatively)
-- of each
elevOfSpot :: Int -> Float -> Int -> Int -> Int -> Int
elevOfSpot v pl dist t 1 = avgElev v t $ normElev pl dist 1 4
elevOfSpot v pl dist t 2 = avgElev v t $ -(normElev pl dist 40 80)
elevOfSpot v pl dist t 3 = avgElev v t $ normElev pl dist 5 6
elevOfSpot v pl dist t 4 = avgElev v t $ normElev pl dist 5 40
elevOfSpot v pl dist t 5 = avgElev v t $ normElev pl dist 30 120

elevOfSpot v pl dist t _ = 0

normElev :: Float -> Int -> Int -> Int -> Int
normElev peaklevel 0 min max = quot (min+max) 2
normElev peaklevel x min max = (quot ((max-min)) ((round peaklevel)))*x + min

avgElev :: Int -> Int -> Int -> Int
avgElev vigor x y = x + (vigor*y)

getElev :: Int -> Float -> [Int] -> Int -> Int -> Int
getElev gridw sealevel e0 x y = do
  let elev = e0 !! (x+(gridw*y))
  round $ (fromIntegral(elev)) - sealevel

formatElev :: Int -> Float -> [Int] -> (Int, Int) -> String
formatElev gridw sealevel e (x, y) = "Elev: " ++ (show (getElev gridw sealevel e x y))

normalizeElevs :: Float -> [Int] -> [Int]
normalizeElevs sl e = map (subSeaLevel sl) e

subSeaLevel :: Float -> Int -> Int
subSeaLevel sealevel e = e - (round sealevel)

-- todo: finish this
blurMap :: State -> [Int] -> Int -> [Int]
blurMap state elev _ = elev
