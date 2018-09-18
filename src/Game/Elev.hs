module Game.Elev where

import Data.List.Split ( chunksOf )
import Game.State
import Game.Settings
import Game.Map

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
      elev0   = map (elevRow state c (fst k) (snd k) (fst j) (snd j)) newelev
      elev1   = stripGrid elev0
      elev2   = flattenGrid elev1
  findElev state c x y g elev2 ks js

elevRow :: State -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
elevRow state c w x y z (t1, t2) = (map (elevTile state c t2 w x y z) t1, t2)

elevTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
elevTile state c j w x y z (t, i)
  | distance i j w x y z t <= maxdist    = (1, i)
  | otherwise                            = (0, i)
  where
    maxdist = 1000 * ((stateSizes state) !! c)


