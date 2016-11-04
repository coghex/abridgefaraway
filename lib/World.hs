module World where

import State
import Draw

buildGrid :: State -> State
buildGrid state = do
  let grid = stateGrid state
      texs = stateTexs state
      gme = stateGame state
      conts = stateConts state
      seeds = stateSeeds state
      rands = stateRands state
      newgrid = seedConts state grid conts 10 seeds rands
      sts = stateTileSizes state
      str = stateTileRands state
  
  State
    { stateGrid = newgrid
    , stateTexs = texs
    , stateGame = gme
    , stateConts = conts
    , stateSeeds = seeds
    , stateRands = rands
    , stateTileSizes = sts
    , stateTileRands = str
    }

seedConts :: State -> [Int] -> [(Int, Int)] -> Int -> [[(Int, Int)]] -> [[(Int, Int)]] -> [Int]
seedConts state g []     _ []     []     = g
seedConts state g _      0 _      _      = g
seedConts state g (l:ls) i (k:ks) (j:js) = do
  let x = seedGrid state i (fst l) (snd l) g k j
  seedConts state x ls (i-1) ks js

seedGrid :: State -> Int -> Int -> Int -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
seedGrid _     _ _ _ g []     []     = g
seedGrid state c x y g (k:ks) (j:js) = do
  let newmap = expandMap g
      map0 = map (seedRow state c (fst k) (snd k) (fst j) (snd j)) newmap
      map3 = stripMap map0
      map4 = flattenMap map3
  seedGrid state c x y map4 ks js

seedRow :: State -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow state c w x y z (t1, t2) = (map (seedTile state c t2 w x y z) t1, t2)

seedTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedTile state c j w x y z (t, i)
  | distance i j w x y z t <= 5000 = (((stateTileRands state) !! c), i)
  | otherwise                      = (t, i)

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 x3 y3 t = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  t*p1*p2

flatIndex :: Int -> Int -> Int
flatIndex x y = (((x) `quot` 120)+((y) `mod` 90))

