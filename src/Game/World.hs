module Game.World where

import Data.List.Split ( chunksOf )
import System.Random
import Game.State
import Game.Settings
import Game.Rand
import Game.Map
import Game.Elev

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
    , stateElev     = (take (gridw*gridh) (repeat 1))
    , stateCursor   = (5,5)
    , stateNConts   = nconts
    , stateCurrMap  = currmap
    , stateConts    = tail conts
    , stateSeeds    = tail $ makeSeeds seeds 0 s1 s2 nspots
    , stateRands    = tail $ makeSeeds rands (2*nconts) s3 s4 nspots
    , stateSizes    = sizes
    , stateTypes    = types
    }
  

initWorld :: State -> Env -> State
initWorld state env = do
  let sg      = stateGame     state
      w       = stateScreenW  state
      h       = stateScreenH  state
      g0      = stateGrid     state
      e0      = stateElev     state
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
  let e1      = elevBlurMap state env g1 e0 conts seeds rands nconts
  let g2      = fixConts state env g1 e1

  State
    { stateGame     = sg
    , stateScreenW  = w 
    , stateScreenH  = h
    , stateGrid     = g2
    , stateElev     = e1
    , stateCursor   = curs
    , stateNConts   = nconts
    , stateCurrMap  = currmap
    , stateConts    = conts
    , stateSeeds    = seeds
    , stateRands    = rands
    , stateSizes    = sizes
    , stateTypes    = types
    }

fixConts :: State -> Env -> [Int] -> [Int] -> [Int]
fixConts state env g e = zipWith fixContSpots g e

fixContSpots :: Int -> Int -> Int
fixContSpots gx ex
  | (((log (fromIntegral ex))/20.0) > sealevel) && (gx == 1)  = 8
  | (((log (fromIntegral ex))/20.0) <= sealevel) && (gx /= 1) = 7
  | otherwise                                            = gx

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
  initWorld newstate env
  

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
  | (randstate > 6) || (randstate < 3)                      = (t, i)
  | distance i j w x y z t <= maxdist                       = (randstate, i)
  | otherwise                                               = (t, i)
  where
    randstate = (stateTypes state) !! c
    maxdist = 1000 * ((stateSizes state) !! c)

