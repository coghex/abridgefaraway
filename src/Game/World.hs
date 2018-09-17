module Game.World where

import Data.List.Split ( chunksOf )
import System.Random
import Game.State
import Game.Settings
import Game.Rand

genParams :: Int -> Int -> StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> State
genParams currmap nconts s1 s2 s3 s4 s5 s6 = do
  let nspots = randomList (minnspots, maxnspots) nconts s1
      conts = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s1), (randomList (fudge, (gridh-fudge)) nconts s2))
      seeds = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s3), (randomList (fudge, (gridh-fudge)) nconts s4))
      rands = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s5), (randomList (fudge, (gridh-fudge)) nconts s6))
      sizes = randomList (minsize, maxsize) nconts s1
      types = randomList (0, 6::Int) nconts s2

  State
    { stateGame     = SMenu
    , stateScreenW  = screenw
    , stateScreenH  = screenh
    , stateGrid     = (take (gridw*gridh) (repeat 1))
    , stateCursor   = (5,5)
    , stateNConts   = nconts
    , stateCurrMap  = currmap
    , stateConts    = tail conts
    , stateSeeds    = tail $ makeSeeds seeds 0 s1 s2 nspots
    , stateRands    = tail $ makeSeeds rands (2*nconts) s3 s4 nspots
    , stateSizes    = sizes
    , stateTypes    = types
    }
  

initWorld :: State -> State
initWorld state = do
  let sg      = stateGame     state
      w       = stateScreenW  state
      h       = stateScreenH  state
      g0      = stateGrid     state
      curs    = stateCursor   state
      nconts  = stateNConts   state
      currmap = stateCurrMap  state
      conts   = stateConts    state
      seeds   = stateSeeds    state
      rands   = stateRands    state
      sizes   = stateSizes    state
      types   = stateTypes    state

  let nconts  = length (stateConts state)
  
  let g1      = seedConts state g0 conts seeds rands nconts

  State
    { stateGame     = sg
    , stateScreenW  = w 
    , stateScreenH  = h
    , stateGrid     = g1
    , stateCursor   = curs
    , stateNConts   = nconts
    , stateCurrMap  = currmap
    , stateConts    = conts
    , stateSeeds    = seeds
    , stateRands    = rands
    , stateSizes    = sizes
    , stateTypes    = types
    }

regenWorld :: State -> Env -> State
regenWorld state env = do
  let s1 = envSeeds env !! 0
      s2 = envSeeds env !! 1
      s3 = envSeeds env !! 2
      s4 = envSeeds env !! 3
      s5 = envSeeds env !! 4
      s6 = envSeeds env !! 5
      currmap = (stateCurrMap state) + 1
      nconts = (randomRs (minnconts, maxnconts) (mkStdGen 42)) !! currmap
  let newstate = genParams currmap nconts s1 s2 s3 s4 s5 s6
  initWorld newstate
  

seedConts :: State -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
seedConts state g []    []    []    _ = g
seedConts state g _     _     _     0 = g
seedConts state g (l:ls) (k:ks) (j:js) i = do
  let x = seedGrid state i (fst l) (snd l) g k j
  seedConts state x ls ks js (i-1)

seedGrid :: State -> Int -> Int -> Int -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
seedGrid _     _ _ _ g []     []     = g
seedGrid state c x y g (k:ks) (j:js) = do
  let newgrid = expandGrid g
      grid0   = map (seedRow state c (fst k) (snd k) (fst j) (snd j)) newgrid
      grid1   = stripGrid grid0
      grid2   = flattenGrid grid1
  seedGrid state c x y grid2 ks js

seedRow :: State -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow state c w x y z (t1, t2) = (map (seedTile state c t2 w x y z) t1, t2)

seedTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedTile state c j w x y z (t, i)
  | (randstate == 1) && (distance i j w x y z t <= maxdist) = (randstate, i)
  | (randstate > 6) || (randstate <3)                       = (t, i)
  | distance i j w x y z t <= maxdist                       = (randstate, i)
  | otherwise                                               = (t, i)
  where
    randstate = (stateTypes state) !! c
    maxdist = 1000 * ((stateSizes state) !! c)

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 x3 y3 t = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1 -x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  p1*p2

expandGrid :: [Int] -> [([(Int, Int)], Int)]
expandGrid m = zip (map workRows (chunksOf gridw m)) [0..gridh]

workRows :: [Int] -> [(Int, Int)]
workRows l = do
  zip l [0..gridw]

flattenGrid :: [[Int]] -> [Int]
flattenGrid xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

stripGrid :: [([(Int, Int)], Int)] -> [[Int]]
stripGrid ((a, b):ys) = (stripRow a) : stripGrid ys
stripGrid _           = [[]]

stripRow :: [(Int, Int)] -> [Int]
stripRow ((a, b):ys) = a : stripRow ys
stripRow _           = []
