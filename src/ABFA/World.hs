module ABFA.World where
-- this generates the world and parameters related to the game state

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Split (chunksOf)
import System.Random
import qualified Foreign.Lua as Lua

import ABFA.State
import ABFA.Settings
import ABFA.Rand
import ABFA.Game
import ABFA.Data
import ABFA.Map
import ABFA.Elev
import ABFA.Zone

-- this will generate parameters for the world generator and place it in a new state
genParams :: Int -> State -> State
genParams mseed state = do
  let stdgens    = stateStdGens          state
      ogsettings = stateSettings         state
      wsettings  = settingWGSettings     ogsettings
      gw         = settingGridW          ogsettings
      gh         = settingGridH          ogsettings
      minns      = wgMinNSpots           wsettings
      maxns      = wgMaxNSpots           wsettings
      wparams    = stateWParams          state
      ncs        = wpNConts              wparams
      ls         = stateLua              state

  State
    { stateGame           = stateGame state
    , stateGamePrev       = stateGamePrev state
    , stateSettings       = ogsettings
    , stateWParams        = genWParams mseed stdgens ogsettings
    , stateStdGens        = stdgens
    , stateZoom           = 120.0
    , stateGrid           = (take (gw*gh) (repeat 1))
    , stateOG             = (take (gw*gh) (repeat 1))
    , stateElev           = (take (gw*gh) (repeat 1))
    , stateCursor         = (5, 5)
    , stateTime           = 0
    , stateLua            = ls
    , stateShellBuff      = [" % ", "welcome to the lua console..."]
    , stateShellInput     = ""
    , stateZone           = []
    }

-- this is used to generate the params data
genWParams :: Int -> [StdGen] -> Settings -> WorldParams
genWParams mseed sgs settings = WorldParams { wpNConts = nconts0
                                      , wpConts  = conts0
                                      , wpSeeds  = seeds0
                                      , wpRands  = rands0
                                      , wpSizes  = sizes0
                                      , wpTypes  = types0
                                      , wpRandI  = randi0
                                      , wpRRands = rrands0 }
  where nconts0   = head (withStdGen sgs 1 (randomRs (minnc, maxnc)))
        nspots    = (withStdGen sgs 1 (randomList (minns, maxns) nconts0))
        conts0    = tail $ buildList2 ((withStdGen sgs 1 (randomList (f, (gw-f)) nconts0)), (withStdGen sgs 2 (randomList (1, (gh-1)) nconts0)))
        seeds0    = tail $ makeSeeds seedsseed nspots sgs f
        seedsseed = buildList2 ((withStdGen sgs 3 (randomList (f, (gw-f)) nconts0)), (withStdGen sgs 4 (randomList (1, (gh-1)) nconts0)))
        rands0    = tail $ makeSeeds randsseed nspots (splitSGs sgs) f
        randsseed = buildList2 ((withStdGen sgs 5 (randomList (f, (gw-f)) nconts0)), (withStdGen sgs 6 (randomList (1, (gh-1)) nconts0)))
        sizes0    = withStdGen sgs 1 $ randomList (mins, maxs) nconts0
        types0    = withStdGen sgs 2 $ randomBiomeList nconts0
        randi0    = mseed
        rrands0   = withStdGen sgs 3 $ randomRs (minnc, maxnc)
        wsettings  = settingWGSettings    settings
        gw         = settingGridW         settings
        gh         = settingGridH         settings
        minns      = wgMinNSpots     wsettings
        maxns      = wgMaxNSpots     wsettings
        minnc      = wgMinNConts     wsettings
        maxnc      = wgMaxNConts     wsettings
        f          = wgFudge         wsettings
        mins       = wgMinSize       wsettings
        maxs       = wgMaxSize       wsettings

-- generates a random list of biomes (only the natural ones)
randomBiomeList :: Int -> StdGen -> [Biome]
randomBiomeList nconts sg = do
  let randoms = randomList (4, 8::Int) nconts sg
  map intToBiome randoms

-- helps genereate parameters for world builder
makeSeeds :: [(Int, Int)] -> [Int] -> [StdGen] -> Int -> [[(Int, Int)]]
makeSeeds []          _              _   _ = []
makeSeeds _           []             _   _ = []
makeSeeds ((x,y):xys) (nspot:nspots) sgs f = (buildList2 ((withStdGen sgs 1 (randomList ((x-f), (x+f)) (nspot))), (withStdGen sgs 2 (randomList ((y-f), (x+f)) (nspot))))):(makeSeeds xys nspots (splitSGs sgs) f)

-- simulates n ticks at a time in history
nextSimState :: State -> Env -> Int -> State
nextSimState state env n = State
  { stateGame       = stateGame       state
  , stateGamePrev   = stateGamePrev   state
  , stateSettings   = stateSettings   state
  , stateWParams    = stateWParams    state
  , stateStdGens    = stateStdGens    state
  , stateZoom       = stateZoom       state
  , stateGrid       = stateGrid       state
  , stateOG         = stateOG         state
  , stateElev       = stateElev       state
  , stateCursor     = stateCursor     state
  , stateTime       = (stateTime      state) + (fromIntegral n)
  , stateLua        = stateLua        state
  , stateShellBuff  = stateShellBuff  state
  , stateShellInput = stateShellInput state
  , stateZone       = stateZone       state
  }

-- animates a single animation frame, returns a new state
animState :: State -> Env -> State
animState state env = state

-- rejects worlds that dont look great
goodWorld :: State -> Bool
goodWorld state
  | (water > (quot (gridw*gridh) 6)) && (water < ((2*(quot (gridw*gridh) 3)))) = True
  | otherwise                                                                  = True--False
  where water      = length waterinmap
        waterinmap = filter (isWater) grid
        grid       = stateGrid state
        gridw      = settingGridW settings
        gridh      = settingGridH settings
        settings   = stateSettings state

-- tells if the current grid tile is water
isWater :: Int -> Bool
isWater 1 = True
isWater _ = False

-- initWorld wrapper to rejectworlds
initGoodWorld :: State -> State
initGoodWorld state = initWorldWithCheck newstate
  where newstate = initWorld state

-- regenWorld wrapper to reject worlds
initWorldWithCheck :: State -> State
initWorldWithCheck state
  | (goodWorld newstate) = newstate
  | otherwise            = initWorldWithCheck newstate
  where newstate = regenWorld state ls
        ls       = stateLua state

-- inits a new world (ie state)
initWorld :: State -> State
initWorld state = do
  let sg        = stateGame       state
      sgp       = stateGamePrev   state
      settings  = stateSettings   state
      wparams   = stateWParams    state
      stdgens   = stateStdGens    state
      zoom      = stateZoom       state
      g0        = stateGrid       state
      og        = stateOG         state
      e0        = stateElev       state
      cursor    = stateCursor     state
      time      = stateTime       state
      conts     = wpConts         wparams
      seeds     = wpSeeds         wparams
      rands     = wpRands         wparams
      nconts    = wpNConts        wparams
      ls        = stateLua        state
      shellbuff = stateShellBuff  state
      shellinp  = stateShellInput state
      zone      = stateZone       state

  let g1 = seedConts   state g0 conts seeds rands nconts
      zr = []
      e1 = e0--elevBlurMap state g1 conts seeds rands nconts e0
      gr = g1--fixConts    state g1 env e1
      er = e1
  State
    { stateGame       = sg
    , stateGamePrev   = sgp
    , stateSettings   = settings
    , stateWParams    = wparams
    , stateStdGens    = stdgens
    , stateZoom       = zoom
    , stateGrid       = gr
    , stateOG         = gr
    , stateElev       = er
    , stateCursor     = cursor
    , stateTime       = time
    , stateLua        = ls
    , stateShellBuff  = shellbuff
    , stateShellInput = shellinp
    , stateZone       = zone
    }

-- regenerates world
regenWorld :: State -> Lua.State -> State
regenWorld state ls = do
  let i        = wpRandI            wparams
      wparams  = stateWParams       state
      settings = stateSettings      state
      wgsetts  = settingWGSettings settings
      s1       = mkStdGen (i+1)
      s2       = mkStdGen (i+2)
      s3       = mkStdGen (i+3)
      s4       = mkStdGen (i+4)
      s5       = mkStdGen (i+5)
      s6       = mkStdGen (i+6)
      sgs      = [s1, s2, s3, s4, s5, s6]
      currmap  = (wgCurrMap wgsetts) + 1
      newstate = initState SLoadWorld ls sgs settings
      stateful = genParams (i+1) newstate
  initWorld stateful

-- creates the continents
seedConts :: State -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
seedConts state g []     []     []     _ = g
seedConts state g _      _      _      0 = g
seedConts state g (l:ls) (k:ks) (j:js) i = do
  let x = seedGrid state 0 i (fst l) (snd l) g k j
  seedConts state x ls ks js (i-1)

-- creates a single continent
seedGrid :: State -> Int -> Int -> Int -> Int -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
seedGrid _     _ _ _ _ g []     []     = g
seedGrid state i c x y g (k:ks) (j:js) = do
  let newgrid  = expandGrid gridw gridh g
      settings = stateSettings state
      gridw    = settingGridW settings
      gridh    = settingGridH settings
      grid0    = parMap rpar (seedRow state c i (fst k) (snd k) (fst j) (snd j)) newgrid
      grid1    = stripGrid   grid0
      grid2    = flattenGrid grid1
  seedGrid state (i+1) c x y grid2 ks js

-- seed a single row of a single continent
seedRow :: State -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow state i c w x y z (t1, t2) = (map (seedTile state i c t2 w x y z) t1, t2)

-- seed a single tile for a single continent
seedTile :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedTile state it c j w x y z (t, i)
  | (randstate == BValley) && (distance i j w x         y         z t < (maxdist-cfudge)*8) = (t, i)
  | (randstate == BValley) && (distance i j w (x+gridw) y         z t < (maxdist-cfudge)*8) = (t, i)
  | (randstate == BValley) && (distance i j w x         (y+gridh) z t < (maxdist-cfudge)*8) = (t, i)
  | (randstate == BValley) && (distance i j w (x-gridw) y         z t < (maxdist-cfudge)*8) = (t, i)
  | (randstate == BValley) && (distance i j w x         (y-gridh) z t < (maxdist-cfudge)*8) = (t, i)
  | (randstate == BValley) && (distance i j w x         y         z t <= (maxdist)*4)       = (r, i)
  | (randstate == BValley) && (distance i j w (x+gridw) y         z t <= (maxdist)*4)       = (r, i)
  | (randstate == BValley) && (distance i j w x         (y+gridh) z t <= (maxdist)*4)       = (r, i)
  | (randstate == BValley) && (distance i j w (x-gridw) y         z t <= (maxdist)*4)       = (r, i)
  | (randstate == BValley) && (distance i j w x         (y-gridh) z t <= (maxdist)*4)       = (r, i)
  | (randstate == BCrags)  && (distance i j w x         y         z t < maxdist-cfudge)     = (t, i)
  | (randstate == BCrags)  && (distance i j w (x+gridw) y         z t < maxdist-cfudge)     = (t, i)
  | (randstate == BCrags)  && (distance i j w x         (y+gridh) z t < maxdist-cfudge)     = (t, i)
  | (randstate == BCrags)  && (distance i j w (x-gridw) y         z t < maxdist-cfudge)     = (t, i)
  | (randstate == BCrags)  && (distance i j w x         (y-gridh) z t < maxdist-cfudge)     = (t, i)
  |                           (distance i j w x         y         z t <= maxdist)           = (r, i)
  |                           (distance i j w (x+gridw) y         z t <= maxdist)           = (r, i)
  |                           (distance i j w x         (y+gridh) z t <= maxdist)           = (r, i)
  |                           (distance i j w (x-gridw) y         z t <= maxdist)           = (r, i)
  |                           (distance i j w x         (y-gridh) z t <= maxdist)           = (r, i)
  | otherwise                                                                               = (t, i)
  where randstate = (wpTypes           wparams) !! c
        r         = calcBiome randstate
        settings  = stateSettings      state
        wparams   = stateWParams       state
        wgsetting = settingWGSettings  settings
        maxdist   = 1000 * ((wpSizes   wparams) !! c)
        cfudge    = 2    * (((wpRRands wparams) !! c) - minnconts)
        minnconts = wgMinNConts        wgsetting
        gridw     = settingGridW       settings
        gridh     = settingGridH       settings
