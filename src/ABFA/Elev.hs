module ABFA.Elev where
-- functions to calculate the elevation of the world map

import ABFA.Rand
import ABFA.State
import ABFA.Data
import ABFA.Map

-- makes sure if elevation < sealevel the tile is underwater
elevOcean :: Float -> Float
elevOcean x
  | x <= (sealevel/(peaklevel)) = 8
  | otherwise                   = x

-- calculate elevation and apply noise
elevBlurMap :: State -> [Int] -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
elevBlurMap state grid elev l k j i = do
  let e1 = elevMap state grid elev l k j i
  blurMap state e1 erosion

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
  let newelev = expandGrid e
      newgrid = expandGrid g
      elev0   = parMap rpar (elevRow state newgrid c (fst k) (snd k) (fst j) (snd j)) newelev
      elev1   = stripGrid elev0
      elev2   = flattenGrid elev1
  findElev state c x y g elev2 ks js

-- find elevation of a row of a continent
elevRow :: State -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
elevRow state g c j w x y z (t, i) = ((elevOf dist t i j g), i)
  where
    maxdist = 1000 * ((wpSizes wparams) !! c)
    dist    = distance i j w x y z t
    wparams = stateWParams state

-- elevation of a single spot
elevOf :: Int -> Int -> Int -> Int -> [([(Int, Int)], Int)] -> Int
elevOf dist t x y g = elevOfSpot dist t typ
  where
    typ = (fst $ (fst (g !! y)) !! x)

-- these are the upper and lower bounds (relatively)
-- of each
elevOfSpot :: Int -> Int -> Int -> Int
elevOfSpot dist t 1 = avgElev t $ normElev dist 1 4
elevOfSpot dist t 2 - avgElev t $ -(normElev dist 40 80)
