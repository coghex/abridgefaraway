module Game.World where

import Data.List.Split ( chunksOf )
import Game.State
import Game.Settings

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 x3 y3 t = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  p1*p2

initWorld :: State -> Int -> State
initWorld state c = do
  let sg   = stateGame      state
      w    = stateScreenW   state
      h    = stateScreenH   state
      g0   = stateGrid      state
      scur = stateCursor    state
      ncs  = stateNConts    state
      is   = stateISpots    state
      nzs  = stateZSpots    state
      ns   = stateNSpots    state
      sc   = stateConts     state
      ss   = stateSeeds     state
      srs  = stateRands     state
      sss  = stateSizes     state
      sts  = stateTypes     state
      ssic = stateSIceConts state
      ssis = stateSIceSizes state
      ssir = stateSIceRands state
      snic = stateNIceConts state
      snis = stateNIceSizes state
      snir = stateNIceRands state
      szc  = stateZazzConts state
      szs  = stateZazzSizes state
      szr  = stateZazzRands state
      szt  = stateZazzTypes state
      zone = stateZone      state
      camx = stateZoneCamx  state
      camy = stateZoneCamy  state
      camz = stateZoneCamz  state
      curz = stateCurrentZ  state
      sb   = stateBushes    state
      sbr  = stateBRands    state
      sbs  = stateBSizes    state
      sps  = statePaths     state
      spr  = statePathRands state
      g1   = seedConts state g0 sc ss srs c
      g2   = addZazz state g1 szc szs szr szt
      g3   = iceGrid state ssis ssic ssir g2
      g4   = iceGrid state snis snic snir g3

  State
    { stateGame      = sg
    , stateScreenW   = w
    , stateScreenH   = h
    , stateGrid      = g4
    , stateCursor    = scur
    , stateNConts    = ncs
    , stateISpots    = is
    , stateZSpots    = nzs
    , stateNSpots    = ns
    , stateConts     = sc
    , stateSeeds     = ss
    , stateRands     = srs
    , stateSizes     = sss
    , stateTypes     = sts
    , stateSIceConts = ssic
    , stateSIceSizes = ssis
    , stateSIceRands = ssir
    , stateNIceConts = snic
    , stateNIceSizes = snis
    , stateNIceRands = snir
    , stateZazzConts = szc
    , stateZazzSizes = szs
    , stateZazzRands = szr
    , stateZazzTypes = szt
    , stateZone      = zone
    , stateZoneCamx  = camx
    , stateZoneCamy  = camy
    , stateZoneCamz  = camz
    , stateCurrentZ  = curz
    , stateBushes    = sb
    , stateBRands    = sbr
    , stateBSizes    = sbs
    , statePaths     = sps
    , statePathRands = spr
    }

seedConts :: State -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
seedConts state g []     []     []     _ = g
seedConts state g _      _      _      0 = g
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
  | (randstate >= 6) || (randstate == 4) = (t, i)
  | distance i j w x y z t <= maxdist    = (randstate, i)
  | otherwise                            = (t, i)
  where
    randstate = (stateTypes state) !! c
    maxdist = 5000 * ((stateSizes state) !! c)

iceGrid :: State -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [Int] -> [Int]
iceGrid _     []         []        []        g = g
iceGrid state ((a, b):ssis) ((c, d):ssi) ((e, f):ssir) g = do
  let newgrid = expandGrid g
      g0      = map (iceRow state c d a b e f) newgrid
      g1 = stripGrid g0
      g2 = flattenGrid g1
  iceGrid state ssis ssi ssir g2

iceRow :: State -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
iceRow state x y sx sy rx ry l = ((map (iceTile state x y sx sy rx ry (snd l)) (fst l)), (snd l))

iceTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
iceTile state x y sx sy rx ry j (t, i)
  | ((j==0) || (j==1) || (j==2))                       = (5, i)
  | (((j==gridh)) || (j==(gridh-1)) || (j==(gridh-2))) = (5, i)
  | (distance i j x y rx ry t) <= (100*sx*sy)          = (5, i)
  | otherwise                                          = (t, i)

addZazz :: State -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [Int] -> [Int]
addZazz _     g []          []           []           []       = g
addZazz state g ((a,b):szs) ((c,d):szss) ((e,f):szrs) (nt:nts) = do
  let newgrid = expandGrid g
      g0 = map (zazzRow state a b c d e f nt) newgrid
      g1 = stripGrid g0
      g2 = flattenGrid g1
  addZazz state g2 szs szss szrs nts

zazzRow :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
zazzRow state a b c d e f t (l, j) = ((map (zazzSpot state j a b c d e f t) l), j)

zazzSpot :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
zazzSpot state _ _ _ _ _ _ _ 5  (t, i) = (t, i)
zazzSpot state j a b c d e f nt (t, i)
  | ((distance i j a b e f t) <= (c*d)) = (nt, i)
  | otherwise                           = (t, i)

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

