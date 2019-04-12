module ABFA.World where
-- this generates the world and parameters related to the game state

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Split (chunksOf)
import System.Random

import ABFA.State
import ABFA.Settings
import ABFA.Rand
import ABFA.Game
import ABFA.Data

-- this will generate parameters for the world generator and place it in a new state
genParams :: State -> State
genParams state = do
  let stdgens    = stateStdGens  state
      ogsettings = stateSettings state
      wsettings  = settingWGSettings     ogsettings
      gw         = settingGridW          ogsettings
      gh         = settingGridH          ogsettings
      minns      = wgMinNSpots           wsettings
      maxns      = wgMaxNSpots           wsettings
      wparams    = stateWParams          state
      ncs        = wpNConts              wparams

  State
    { stateGame           = SMenu
    , stateSettings       = ogsettings
    , stateWParams        = genWParams stdgens ogsettings
    , stateStdGens        = stdgens
    , stateZoom           = 120.0
    , stateGrid           = (take (gw*gh) (repeat 1))
    , stateOG             = (take (gw*gh) (repeat 1))
    , stateElev           = (take (gw*gh) (repeat 1))
    , stateCursor         = (5, 5)
    , stateTime           = 0
    }

-- this is used to generate the params data
genWParams :: [StdGen] -> Settings -> WorldParams
genWParams sgs settings = WorldParams { wpNConts = nconts0
                                      , wpConts  = conts0
                                      , wpSeeds  = seeds0
                                      , wpRands  = rands0
                                      , wpSizes  = sizes0
                                      , wpTypes  = types0
                                      , wpRandI  = randi0
                                      , wpRRands = rrands0 }
  where nconts0   = (withStdGen sgs 1 (randomRs (minnc, maxnc))) !! 1
        nspots    = (withStdGen sgs 1 (randomList (minns, maxns) nconts0))
        conts0    = tail $ buildList2 ((withStdGen sgs 1 (randomList (f, (gw-f)) nconts0)), (withStdGen sgs 2 (randomList (1, (gh-1)) nconts0)))
        seeds0    = tail $ makeSeeds seedsseed nspots sgs f
        seedsseed = buildList2 ((withStdGen sgs 3 (randomList (f, (gw-f)) nconts0)), (withStdGen sgs 4 (randomList (1, (gh-1)) nconts0)))
        rands0    = tail $ makeSeeds randsseed nspots (splitSGs sgs) f
        randsseed = buildList2 ((withStdGen sgs 5 (randomList (f, (gw-f)) nconts0)), (withStdGen sgs 6 (randomList (1, (gh-1)) nconts0)))
        sizes0    = withStdGen sgs 1 $ randomList (mins, maxs) nconts0
        types0    = withStdGen sgs 2 $ randomList (mins, maxs) nconts0
        randi0    = 0
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

-- helps genereate parameters for world builder
makeSeeds :: [(Int, Int)] -> [Int] -> [StdGen] -> Int -> [[(Int, Int)]]
makeSeeds []          _              _   _ = []
makeSeeds _           []             _   _ = []
makeSeeds ((x,y):xys) (nspot:nspots) sgs f = (buildList2 ((withStdGen sgs 1 (randomList ((x-f), (x+f)) (nspot))), (withStdGen sgs 2 (randomList ((y-f), (x+f)) (nspot))))):(makeSeeds xys nspots (splitSGs sgs) f)

-- simulates n ticks at a time in history
nextSimState :: State -> Env -> Int -> State
nextSimState state env n = State
  { stateGame       = stateGame state
  , stateSettings   = stateSettings state
  , stateWParams    = stateWParams  state
  , stateStdGens    = stateStdGens  state
  , stateZoom       = stateZoom     state
  , stateGrid       = stateGrid     state
  , stateOG         = stateOG       state
  , stateElev       = stateElev     state
  , stateCursor     = stateCursor   state
  , stateTime       = stateTime     state
  }

-- animates a single animation frame, returns a new state
animState :: State -> Env -> State
animState state env = state
