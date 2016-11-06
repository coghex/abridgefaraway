module World where

import State
import Draw
import Settings

zazzGrid :: State -> State
zazzGrid state = do
  let grid = stateGrid state
      texs = stateTexs state
      gme = stateGame state
      conts = stateConts state
      seeds = stateSeeds state
      rands = stateRands state
      sts = stateTileSizes state
      str = stateTileRands state
      scs = stateContSizes state
      sis = stateSIceSizes state
      si = stateSIces state
      sir = stateSIceRands state
      nsis = stateNIceSizes state
      nsi = stateNIces state
      nsir = stateNIceRands state
      zs = stateZazzs state
      zss = stateZazzSizes state
      zrs = stateZazzRands state
      zts = stateZazzTypes state
      scursor = stateCursor state
      newgrid = addZazz state grid zs zss zrs zts
  
  State
    { stateGrid = newgrid
    , stateTexs = texs
    , stateGame = gme
    , stateConts = conts
    , stateSeeds = seeds
    , stateRands = rands
    , stateTileSizes = sts
    , stateTileRands = str
    , stateContSizes = scs
    , stateSIceSizes = sis
    , stateSIces = si
    , stateSIceRands = sir
    , stateNIceSizes = nsis
    , stateNIces = nsi
    , stateNIceRands = nsir
    , stateZazzs = zs
    , stateZazzSizes = zss
    , stateZazzRands = zrs
    , stateZazzTypes = zts
    , stateCursor = scursor
    }

addZazz :: State -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [Int] -> [Int]
addZazz _     g []          []           []           []       = g
addZazz state g ((a,b):szs) ((c,d):szss) ((e,f):szrs) (nt:nts) = do
  let newmap = expandMap g
      map0 = map (zazzRow state a b c d e f nt) newmap
      map3 = stripMap map0
      map4 = flattenMap map3
  addZazz state map4 szs szss szrs nts

zazzRow :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
zazzRow state a b c d e f t (l, j) = ((map (zazzSpot state j a b c d e f t) l), j)

zazzSpot :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
zazzSpot state _ _ _ _ _ _ _ 6  (t, i)  = (t, i)
zazzSpot state _ _ _ _ _ _ _ 7  (t, i)  = (t, i)
zazzSpot state _ _ _ _ _ _ _ 8  (t, i)  = (t, i)
zazzSpot state j a b c d e f nt (t, i)
  | ((distance i j a b e f t) <= (c*d)) = (nt, i)
  | otherwise                           = (t, i)

iceGrid :: State -> State
iceGrid state = do
  let grid = stateGrid state
      texs = stateTexs state
      gme = stateGame state
      conts = stateConts state
      seeds = stateSeeds state
      rands = stateRands state
      sts = stateTileSizes state
      str = stateTileRands state
      scs = stateContSizes state
      sis = stateSIceSizes state
      si = stateSIces state
      sir = stateSIceRands state
      nsis = stateNIceSizes state
      nsi = stateNIces state
      nsir = stateNIceRands state
      zs = stateZazzs state
      zss = stateZazzSizes state
      zrs = stateZazzRands state
      zts = stateZazzTypes state
      scursor = stateCursor state
      newgrid = iceMap state nsis nsi nsir (iceMap state sis si sir grid)
  
  State
    { stateGrid = newgrid
    , stateTexs = texs
    , stateGame = gme
    , stateConts = conts
    , stateSeeds = seeds
    , stateRands = rands
    , stateTileSizes = sts
    , stateTileRands = str
    , stateContSizes = scs
    , stateSIceSizes = sis
    , stateSIces = si
    , stateSIceRands = sir
    , stateNIceSizes = nsis
    , stateNIces = nsi
    , stateNIceRands = nsir
    , stateZazzs = zs
    , stateZazzSizes = zss
    , stateZazzRands = zrs
    , stateZazzTypes = zts
    , stateCursor = scursor
    }

iceMap :: State -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [Int] -> [Int]
iceMap _     []         []       []         g = g
iceMap state ((a,b):ssis) ((c,d):ssi) ((e,f):ssir) g = do
  let newmap = expandMap g
      map0 = map (iceRow state c d a b e f) newmap
      map3 = stripMap map0
      map4 = flattenMap map3
  iceMap state ssis ssi ssir map4

iceRow :: State -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
iceRow state x y sx sy rx ry l = ((map (iceTile state x y sx sy rx ry (snd l)) (fst l)), (snd l))

iceTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
iceTile state x y sx sy rx ry j (t, i)
  | ((j==0) || (j==1) || (j==2) || (j==88) || (j==89) || (j==88)) = (7, i)
  | (distance i j x y rx ry t) <= (100*sx*sy)                     = (7, i)
  | otherwise                                                     = (t, i)

buildGrid :: State -> Int -> State
buildGrid state c = do
  let grid = stateGrid state
      texs = stateTexs state
      gme = stateGame state
      conts = stateConts state
      seeds = stateSeeds state
      rands = stateRands state
      newgrid = seedConts state grid conts c seeds rands
      sts = stateTileSizes state
      str = stateTileRands state
      scs = stateContSizes state
      sis = stateSIceSizes state
      si = stateSIces state
      sir = stateSIceRands state
      nsis = stateNIceSizes state
      nsi = stateNIces state
      nsir = stateNIceRands state
      zs = stateZazzs state
      zss = stateZazzSizes state
      zrs = stateZazzRands state
      zts = stateZazzTypes state
      scursor = stateCursor state
  
  State
    { stateGrid = newgrid
    , stateTexs = texs
    , stateGame = gme
    , stateConts = conts
    , stateSeeds = seeds
    , stateRands = rands
    , stateTileSizes = sts
    , stateTileRands = str
    , stateContSizes = scs
    , stateSIceSizes = sis
    , stateSIces = si
    , stateSIceRands = sir
    , stateNIceSizes = nsis
    , stateNIces = nsi
    , stateNIceRands = nsir
    , stateZazzs = zs
    , stateZazzSizes = zss
    , stateZazzRands = zrs
    , stateZazzTypes = zts
    , stateCursor = scursor
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
  | (randstate >= 6) || (randstate == 4) = (t, i)
  | distance i j w x y z t <= maxdist    = (randstate, i)
  | otherwise                            = (t, i)
  where
    randstate = (stateTileRands state) !! c
    maxdist = 1000 * (fst ((stateContSizes state) !! c))

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 x3 y3 t = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  p1*p2

flatIndex :: Int -> Int -> Int
flatIndex x y = (((x) `quot` gridw)+((y) `mod` gridh))

