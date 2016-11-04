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
      newgrid = seedConts state grid conts
  
  State
    { stateGrid = newgrid
    , stateTexs = texs
    , stateGame = gme
    , stateConts = conts
    , stateSeeds = seeds
    , stateRands = rands
    }

seedConts :: State -> [Int] -> [(Int, Int)] -> [Int]
seedConts state g []     = g
seedConts state g (l:ls) = do
  let x = seedGrid state (fst l) (snd l) g
  seedConts state x ls

seedGrid :: State -> Int -> Int -> [Int] -> [Int]
seedGrid state x y l = do
  let newmap = expandMap l
      map0 = map (seedRow state x y) newmap
      map3 = stripMap map0
      map4 = flattenMap map3
  map4

seedRow :: State -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow state x y l = ((map (seedTile state x y (snd l)) (fst l)), (snd l))

seedTile :: State -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedTile state x y j (t, i)
  | ((distance x y i j t) <= 1000) = (2, i)
  | otherwise                      = (t, i)

distance :: Int -> Int -> Int -> Int -> Int -> Int
distance x y i j t = (((x-i)*(x-i))+((y-j)*(y-j)))
