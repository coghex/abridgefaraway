module Game.World where

import Data.List.Split ( chunksOf )
import System.Random
import Game.State
import Game.Settings
import Game.Rand
import Game.Map
import Game.Elev
import Game.Sun

genParams :: GameState -> Int -> Int -> Int -> [Int] -> Sun -> StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> State
genParams gs currmap nconts i rangers sol s1 s2 s3 s4 s5 s6 = do
  let nspots = randomList (minnspots, maxnspots) nconts s1
      conts = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s1), (randomList (1, (gridh-1)) nconts s2))
      seeds = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s3), (randomList (1, (gridh-1)) nconts s4))
      rands = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s5), (randomList (1, (gridh-1)) nconts s6))
      sizes = randomList (minsize, maxsize) nconts s1
      types = randomList (0, 6::Int) nconts s2

  State
    { stateGame       = gs
    , stateStdGens    = [s1, s2, s3, s4, s5, s6]
    , stateScreenW    = screenw
    , stateScreenH    = screenh
    , stateGrid       = (take (gridw*gridh) (repeat 1))
    , stateElev       = (take (gridw*gridh) (repeat 1))
    , stateCursor     = (5,5)
    , stateNConts     = nconts
    , stateCurrMap    = currmap
    , stateConts      = tail conts
    , stateSeeds      = tail $ makeSeeds seeds 0 s1 s2 nspots
    , stateRands      = tail $ makeSeeds rands (2*nconts) s3 s4 nspots
    , stateSizes      = sizes
    , stateTypes      = types
    , stateRandI      = i
    , stateRangeRands = rangers
    , stateSun        = sol
    , stateSunSpots   = theBigSpotter sol
    , stateTime       = 0
    , stateOceans     = []
    , stateSkies      = []
    }
  

initWorld :: State -> Env -> State
initWorld state env = do
  let sg      = stateGame       state
      stdgens = stateStdGens    state
      w       = stateScreenW    state
      h       = stateScreenH    state
      g0      = stateGrid       state
      e0      = stateElev       state
      curs    = stateCursor     state
      nconts  = stateNConts     state
      currmap = stateCurrMap    state
      conts   = stateConts      state
      seeds   = stateSeeds      state
      rands   = stateRands      state
      sizes   = stateSizes      state
      types   = stateTypes      state
      randi   = stateRandI      state
      rangers = stateRangeRands state
      sun     = stateSun        state
      sunspot = stateSunSpots   state
      time    = stateTime       state
      oceans  = stateOceans     state
      skies   = stateSkies      state

  let nconts  = length (stateConts state)
  
  let g1      = seedConts state g0 conts seeds rands nconts
  let e1      = elevBlurMap state g1 e0 conts seeds rands nconts
  let g2      = fixConts state env g1 e1

  State
    { stateGame       = sg
    , stateStdGens    = stdgens
    , stateScreenW    = w 
    , stateScreenH    = h
    , stateGrid       = g2
    , stateElev       = e1
    , stateCursor     = curs
    , stateNConts     = nconts
    , stateCurrMap    = currmap
    , stateConts      = conts
    , stateSeeds      = seeds
    , stateRands      = rands
    , stateSizes      = sizes
    , stateTypes      = types
    , stateRandI      = randi
    , stateRangeRands = rangers
    , stateSun        = sun
    , stateSunSpots   = sunspot
    , stateTime       = time
    , stateOceans     = oceans
    , stateSkies      = skies
    }

nextState :: State -> Env -> State
nextState state env = State
    { stateGame       = stateGame state
    , stateStdGens    = stateStdGens state
    , stateScreenW    = stateScreenW state
    , stateScreenH    = stateScreenH state
    , stateGrid       = stateGrid state
    , stateElev       = stateElev state
    , stateCursor     = stateCursor state
    , stateNConts     = stateNConts state
    , stateCurrMap    = stateCurrMap state
    , stateConts      = stateConts state
    , stateSeeds      = stateSeeds state
    , stateRands      = stateRands state
    , stateSizes      = stateSizes state
    , stateTypes      = stateTypes state
    , stateRandI      = stateRandI state
    , stateRangeRands = stateRangeRands state
    , stateSun        = sun
    , stateSunSpots   = theBigSpotter sun
    , stateTime       = time
    , stateOceans     = stateOceans state
    , stateSkies      = stateSkies state }
  where
    time   = (stateTime state)+1
    sun    = moveSun oldsun time
    oldsun = stateSun state

fixConts :: State -> Env -> [Int] -> [Int] -> [Int]
fixConts state env g e = zipWith fixContSpots g e

fixContSpots :: Int -> Int -> Int
fixContSpots gx ex
  | (((fromIntegral ex)) < 0)                           = 12
  | (((fromIntegral ex)) > sealevel) && (gx == 1)       = 8
  | (((fromIntegral ex)) <= sealevel) && (gx /= 1)      = 7
  | (((fromIntegral ex)) >= peaklevel)                  = 9
  | (((fromIntegral ex)) > (sealevel*0.9)) && (gx == 1) = 7
  | otherwise                                                                                         = gx

regenWorld :: State -> Env -> State
regenWorld state env = do
  let i  = stateRandI state
      s1 = mkStdGen $ envSeeds env !! i+1
      s2 = mkStdGen $ envSeeds env !! i+2
      s3 = mkStdGen $ envSeeds env !! i+3
      s4 = mkStdGen $ envSeeds env !! i+4
      s5 = mkStdGen $ envSeeds env !! i+5
      s6 = mkStdGen $ envSeeds env !! i+6
      currmap  = (stateCurrMap state) + 1
      nconts   = (randomRs (minnconts, maxnconts) (mkStdGen (42+i))) !! currmap
      rangers  = (randomRs (10000, 100000) (mkStdGen (42+i)))
      sol      = (makeSun 0.0 (fromIntegral(gridh)/2) 800 600)
  let newstate = genParams SLoad currmap nconts (i+1) rangers sol s1 s2 s3 s4 s5 s6
  initWorld newstate env

genStep :: StdGen -> StdGen
genStep g = snd $ next g

seedConts :: State -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
seedConts state g []    []    []    _ = g
seedConts state g _     _     _     0 = g
seedConts state g (l:ls) (k:ks) (j:js) i = do
  let x = seedGrid state 0 i (fst l) (snd l) g k j
  seedConts state x ls ks js (i-1)

seedGrid :: State -> Int -> Int -> Int -> Int -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
seedGrid _     _ _ _ _ g []     []     = g
seedGrid state i c x y g (k:ks) (j:js) = do
  let newgrid = expandGrid g
      grid0   = map (seedRow state c i (fst k) (snd k) (fst j) (snd j)) newgrid
      grid1   = stripGrid grid0
      grid2   = flattenGrid grid1
  seedGrid state (i+1) c x y grid2 ks js

seedRow :: State -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow state c i w x y z (t1, t2) = (map (seedTile state c i t2 w x y z) t1, t2)

seedTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedTile state c it j w x y z (t, i)
  | (randstate == 1) && (distance i j w x y z t <= maxdist)           = (randstate, i)
  | (randstate > 6) || (randstate < 2)                                = (t, i)
  | (randstate == 2) && (distance i j w x y z t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (distance i j w x y z t <= (4*maxdist))       = (randstate, i)
  | (randstate == 5) && (distance i j w x y z t < maxdist-cfudge)     = (t, i)
  | distance i j w x y z t <= maxdist                                 = (randstate, i)
  | otherwise                                                         = (t, i)
  where
    randstate = (stateTypes state) !! c
    maxdist = 1000 * ((stateSizes state) !! c)
    cfudge = 2 * (((stateRangeRands state) !! (c)) - minnconts)

