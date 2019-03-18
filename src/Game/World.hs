module Game.World where

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Split ( chunksOf )
import System.Random
import Game.State
import Game.Settings
import Game.Rand
import Game.Map
import Game.Elev
import Game.Sun
import Game.Moon
import Game.Ocean
import Game.Sky
import Game.Unit
import Game.Data
import Game.Volc
import Game.Civ

genParams :: GameState -> Int -> Int -> Int -> [Int] -> Sun -> Moon -> StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> State
genParams gs currmap nconts i rangers sol luna s1 s2 s3 s4 s5 s6 = do
  let nspots = randomList (minnspots, maxnspots) nconts s1
      conts = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s1), (randomList (1, (gridh-1)) nconts s2))
      seeds = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s3), (randomList (1, (gridh-1)) nconts s4))
      rands = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s5), (randomList (1, (gridh-1)) nconts s6))
      sizes = randomList (minsize, maxsize) nconts s1
      types = randomList (0, 6::Int) nconts s2

  State
    { stateGame           = gs
    , stateStdGens        = [s1, s2, s3, s4, s5, s6]
    , stateScreenW        = screenw
    , stateScreenH        = screenh
    , stateZoom           = 120.0
    , stateGrid           = (take (gridw*gridh) (repeat 1))
    , stateOG             = (take (gridw*gridh) (repeat 1))
    , stateElev           = (take (gridw*gridh) (repeat 1))
    , stateCursor         = (5,5)
    , stateNConts         = nconts
    , stateCurrMap        = currmap
    , stateConts          = tail conts
    , stateSeeds          = tail $ makeSeeds seeds 0 s1 s2 nspots
    , stateRands          = tail $ makeSeeds rands (2*nconts) s3 s4 nspots
    , stateSizes          = sizes
    , stateTypes          = types
    , stateRandI          = i
    , stateRangeRands     = rangers
    , stateSun            = sol
    , stateMoon           = luna
    , stateSunSpots       = theBigSpotter sol (moonSpotter luna)
    , stateTime           = 0
    , stateOceans         = []
    , stateOceanTempZ     = 1
    , stateOceanCurrentsZ = 1
    , stateSkies          = []
    , stateSkyTempZ       = 1
    , stateWindZ          = 1
    , stateRainZ          = 1
    , stateVolcanism      = []
    , stateDesireability  = []
    , stateZones          = []
    , stateUnits          = []
    }

isWater :: Int -> Bool
isWater 1  = True
isWater 7  = True
isWater 12 = True
isWater _  = False

goodWorld :: State -> Bool
goodWorld state
  | (water > (quot (gridh*gridw) 6)) && (water < ((2*(quot (gridh*gridw) 3)))) = True
  | otherwise                                                              = False
  where water      = length waterinmap
        waterinmap = filter (isWater) grid
        grid       = stateGrid state

initGoodWorld :: State -> Env -> State
initGoodWorld state env = initWorldWithCheck newstate env
  where newstate = initWorld state env

initWorldWithCheck :: State -> Env -> State
initWorldWithCheck state env
  | (goodWorld nextstate) = nextstate
  | otherwise             = initWorldWithCheck nextstate env
  where nextstate = regenWorld state env

initWorld :: State -> Env -> State
initWorld state env = do
  let sg      = stateGame           state
      stdgens = stateStdGens        state
      w       = stateScreenW        state
      h       = stateScreenH        state
      zoom    = stateZoom           state
      g0      = stateGrid           state
      og      = stateOG             state
      e0      = stateElev           state
      curs    = stateCursor         state
      nconts  = stateNConts         state
      currmap = stateCurrMap        state
      conts   = stateConts          state
      seeds   = stateSeeds          state
      rands   = stateRands          state
      sizes   = stateSizes          state
      types   = stateTypes          state
      randi   = stateRandI          state
      rangers = stateRangeRands     state
      sun     = stateSun            state
      moon    = stateMoon           state
      sunspot = stateSunSpots       state
      time    = stateTime           state
      oceans  = stateOceans         state
      seaz    = stateOceanTempZ     state
      seacz   = stateOceanCurrentsZ state
      skies   = stateSkies          state
      skyz    = stateSkyTempZ       state
      windz   = stateWindZ          state
      rainz   = stateRainZ          state
      volc    = stateVolcanism      state
      des     = stateDesireability  state
      zones   = stateZones          state
      units   = stateUnits          state

  let nconts  = length (stateConts state)
  
  let g1      = seedConts state g0 conts seeds rands nconts
  let e1      = elevBlurMap state g1 e0 conts seeds rands nconts
  let g2      = fixConts state env g1 e1
  let o1      = theGreatSeas g2 e1 sunspot
  let g3      = iceMap state env g2 o1 g2
  let s1      = theExpanseAbove o1 g3 e1 sunspot
  let v1      = volcanate state env g3 e1
  let testmove0 = MoveTo { dest     = (30, 24)
                         , speed    = 1.5 }
  let testminion0 = Unit { unittexs = (envUnitTex  env) !! 2
                         , frame    = 0
                         , unittype = 1
                         , action   = testmove0
                         , zone     = (10, 10)
                         , position = (24, 24)
                         , dir      = 7 }
  let testmove1 = MoveTo { dest     = (24, 22)
                         , speed    = 1.0 }
  let testminion1 = Unit { unittexs = (envUnitTex  env) !! 2
                         , frame    = 0
                         , unittype = 1
                         , action   = testmove1
                         , zone     = (10, 10)
                         , position = (30, 22)
                         , dir      = 8 }

  let testminionnidle = Unit { unittexs = (envUnitTex env) !! 1
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (26,21)
                        , dir      = 1 }
  let testminionsidle = Unit { unittexs = (envUnitTex env) !! 2
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (26,19)
                        , dir      = 2 }
  let testminioneidle = Unit { unittexs = (envUnitTex env) !! 3
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (27,20)
                        , dir      = 3 }
  let testminionwidle = Unit { unittexs = (envUnitTex env) !! 4
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (25,20)
                        , dir      = 4 }
  let testminionn = Unit { unittexs = (envUnitTex env) !! 5
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (31,21)
                        , dir      = 5 }
  let testminions = Unit { unittexs = (envUnitTex env) !! 6
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (31,19)
                        , dir      = 6 }
  let testminione = Unit { unittexs = (envUnitTex env) !! 7
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (32,20)
                        , dir      = 7 }
  let testminionw = Unit { unittexs = (envUnitTex env) !! 8
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (30,20)
                        , dir      = 8 }
  let testminionne = Unit { unittexs = (envUnitTex env) !! 9
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (30,21)
                        , dir      = 9 }
  let testminionnw = Unit { unittexs = (envUnitTex env) !! 10
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (32,21)
                        , dir      = 10 }
  let testminionse = Unit { unittexs = (envUnitTex env) !! 11
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (30,19)
                        , dir      = 11 }
  let testminionsw = Unit { unittexs = (envUnitTex env) !! 12
                        , frame    = 0
                        , unittype = 1
                        , action   = NullAction
                        , zone     = (10,10)
                        , position = (32,19)
                        , dir      = 12 }
  State
    { stateGame           = sg
    , stateStdGens        = stdgens
    , stateScreenW        = w 
    , stateScreenH        = h
    , stateZoom           = zoom
    , stateGrid           = g3
    , stateOG             = g2
    , stateElev           = e1
    , stateCursor         = curs
    , stateNConts         = nconts
    , stateCurrMap        = currmap
    , stateConts          = conts
    , stateSeeds          = seeds
    , stateRands          = rands
    , stateSizes          = sizes
    , stateTypes          = types
    , stateRandI          = randi
    , stateRangeRands     = rangers
    , stateSun            = sun
    , stateMoon           = moon
    , stateSunSpots       = sunspot
    , stateTime           = time
    , stateOceans         = o1
    , stateOceanTempZ     = seaz
    , stateOceanCurrentsZ = seacz
    , stateSkies          = s1
    , stateSkyTempZ       = skyz
    , stateWindZ          = windz
    , stateRainZ          = rainz
    , stateVolcanism      = v1
    , stateDesireability  = des
    , stateZones          = zones
    , stateUnits          = [testminion0, testminion1]
    }

nextSimState :: State -> Env -> State
nextSimState state env = State
    { stateGame           = stateGame state
    , stateStdGens        = stateStdGens state
    , stateScreenW        = stateScreenW state
    , stateScreenH        = stateScreenH state
    , stateZoom           = stateZoom state
    , stateGrid           = grid
    , stateOG             = stateOG state
    , stateElev           = stateElev state
    , stateCursor         = stateCursor state
    , stateNConts         = stateNConts state
    , stateCurrMap        = stateCurrMap state
    , stateConts          = stateConts state
    , stateSeeds          = stateSeeds state
    , stateRands          = stateRands state
    , stateSizes          = stateSizes state
    , stateTypes          = stateTypes state
    , stateRandI          = stateRandI state
    , stateRangeRands     = stateRangeRands state
    , stateSun            = sun
    , stateMoon           = moon
    , stateSunSpots       = sspots
    , stateTime           = time
    , stateOceans         = newos
    , stateOceanTempZ     = stateOceanTempZ state
    , stateOceanCurrentsZ = stateOceanCurrentsZ state
    , stateSkies          = newss
    , stateSkyTempZ       = stateSkyTempZ state
    , stateWindZ          = stateWindZ state
    , stateRainZ          = stateRainZ state
    , stateVolcanism      = stateVolcanism state
    , stateDesireability  = des
    , stateZones          = stateZones state
    , stateUnits          = stateUnits state }
  where
    time    = (stateTime state)+60
    sun     = moveSun oldsun time
    moon    = moveMoon oldmoon time
    oldsun  = stateSun state
    oldmoon = stateMoon state
    newos   = tempOcean (stateOceans state) sspots mspots
    newss   = tempSky (stateSkies state) (stateOceans state) (stateElev state) sspots mspots
    sspots  = theBigSpotter sun mspots
    mspots  = moonSpotter moon
    grid    = iceMap state env (stateGrid state) (stateOceans state) (stateOG state)
    des     = calcDesireability state env


nextState :: State -> Env -> State
nextState state env = State
    { stateGame           = stateGame state
    , stateStdGens        = stateStdGens state
    , stateScreenW        = stateScreenW state
    , stateScreenH        = stateScreenH state
    , stateZoom           = stateZoom state
    , stateGrid           = grid
    , stateOG             = stateOG state
    , stateElev           = stateElev state
    , stateCursor         = stateCursor state
    , stateNConts         = stateNConts state
    , stateCurrMap        = stateCurrMap state
    , stateConts          = stateConts state
    , stateSeeds          = stateSeeds state
    , stateRands          = stateRands state
    , stateSizes          = stateSizes state
    , stateTypes          = stateTypes state
    , stateRandI          = stateRandI state
    , stateRangeRands     = stateRangeRands state
    , stateSun            = sun
    , stateMoon           = moon
    , stateSunSpots       = sspots
    , stateTime           = time
    , stateOceans         = newos
    , stateOceanTempZ     = stateOceanTempZ state
    , stateOceanCurrentsZ = stateOceanCurrentsZ state
    , stateSkies          = newss
    , stateSkyTempZ       = stateSkyTempZ state
    , stateWindZ          = stateWindZ state
    , stateRainZ          = stateRainZ state
    , stateVolcanism      = stateVolcanism state
    , stateDesireability  = des
    , stateZones          = stateZones state
    , stateUnits          = stateUnits state }
  where
    time    = (stateTime state)+1
    sun     = moveSun oldsun time
    moon    = moveMoon oldmoon time
    oldsun  = stateSun state
    oldmoon = stateMoon state
    newos   = tempOcean (stateOceans state) sspots mspots
    newss   = tempSky (stateSkies state) (stateOceans state) (stateElev state) sspots mspots
    sspots  = theBigSpotter sun mspots
    mspots  = moonSpotter moon
    grid    = iceMap state env (stateGrid state) (stateOceans state) (stateOG state)
    units   = evalUnits state env
    des     = calcDesireability state env

fixConts :: State -> Env -> [Int] -> [Int] -> [Int]
fixConts state env g e = parZipWith fixContSpots g e

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
      luna     = (makeMoon 0.0 (fromIntegral(gridh)/2) 20 10)
  let newstate = genParams SLoad currmap nconts (i+1) rangers sol luna s1 s2 s3 s4 s5 s6
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
      grid0   = parMap rpar (seedRow state c i (fst k) (snd k) (fst j) (snd j)) newgrid
      grid1   = stripGrid grid0
      grid2   = flattenGrid grid1
  seedGrid state (i+1) c x y grid2 ks js

seedRow :: State -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow state i c w x y z (t1, t2) = (map (seedTile state i c t2 w x y z) t1, t2)

seedTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedTile state it c j w x y z (t, i)
  | (randstate == 1) && (distance i j w x         y         z t <= maxdist)           = (randstate, i)
  | (randstate == 1) && (distance i j w (x+gridw) y         z t <= maxdist)           = (randstate, i)
  | (randstate == 1) && (distance i j w x         (y+gridh) z t <= maxdist)           = (randstate, i)
  | (randstate == 1) && (distance i j w (x-gridw) y         z t <= maxdist)           = (randstate, i)
  | (randstate == 1) && (distance i j w x         (y-gridh) z t <= maxdist)           = (randstate, i)
  | (randstate > 6) || (randstate < 2)                                                = (t, i)
  | (randstate == 2) && (distance i j w x         y         z t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (distance i j w (x+gridw) y         z t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (distance i j w x         (y+gridw) z t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (distance i j w (x-gridw) y         z t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (distance i j w x         (y-gridw) z t < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (distance i j w x         y         z t <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (distance i j w (x+gridw) y         z t <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (distance i j w x         (y+gridh) z t <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (distance i j w (x-gridw) y         z t <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (distance i j w x         (y-gridh) z t <= (4*maxdist))       = (randstate, i)
  | (randstate == 5) && (distance i j w x         y         z t < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (distance i j w (x+gridw) y         z t < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (distance i j w x         (y+gridh) z t < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (distance i j w (x-gridw) y         z t < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (distance i j w x         (y-gridh) z t < maxdist-cfudge)     = (t, i)
  | distance i j w x         y         z t <= maxdist                                 = (randstate, i)
  | distance i j w (x+gridw) y         z t <= maxdist                                 = (randstate, i)
  | distance i j w x         (y+gridh) z t <= maxdist                                 = (randstate, i)
  | distance i j w (x-gridw) y         z t <= maxdist                                 = (randstate, i)
  | distance i j w x         (y-gridh) z t <= maxdist                                 = (randstate, i)
  | otherwise                                                                         = (t, i)
  where
    randstate = (stateTypes state) !! c
    maxdist = 1000 * ((stateSizes state) !! c)
    cfudge = 2 * (((stateRangeRands state) !! (c)) - minnconts)

