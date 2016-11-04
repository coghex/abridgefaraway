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
      newgrid = seedConts state grid conts 10
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

seedConts :: State -> [Int] -> [(Int, Int)] -> Int -> [Int]
seedConts state g []     _ = g
seedConts state g _      0 = g
seedConts state g (l:ls) i = do
  let x = seedGrid state i (fst l) (snd l) g
  seedConts state x ls (i-1)

seedGrid :: State -> Int -> Int -> Int -> [Int] -> [Int]
seedGrid state i x y l = do
  let newmap = expandMap l
      map0 = map (seedRow state i x y) newmap
      map3 = stripMap map0
      map4 = flattenMap map3
  map4

seedRow :: State -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow state i x y l = ((map (seedTile state i x y (snd l)) (fst l)), (snd l))

seedTile :: State -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedTile state c x y j (t, i)
  | (distance x y i j t) <= (10*(stateTileSizes state) !! (flatIndex x y)) = (((stateTileRands state) !! c), i)
  | otherwise                                                              = (t, i)

distance :: Int -> Int -> Int -> Int -> Int -> Int
distance x y i j t = t*(((x-i)*(x-i))+((y-j)*(y-j)))

flatIndex :: Int -> Int -> Int
flatIndex x y = (((x) `quot` 120)+((y) `mod` 90))

