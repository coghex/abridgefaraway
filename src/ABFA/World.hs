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

-- this will generate parameters for the world generator and place it in a new state
genParams :: State -> State
genParams state = do
  let nspots  = randomList (minnspots, maxnspots) nconts s1
      conts   = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s1), (randomList (1, (gridh-1)) nconts s2))
      seeds   = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s3), (randomList (1, (gridh-1)) nconts s4))
      rands   = buildList2 ((randomList (fudge, (gridw-fudge)) nconts s5), (randomList (1, (gridh-1)) nconts s6))
      sizes   = randomList (minsize, maxsize) nconts s1
      types   = randomList (0, 6::Int) nconts s2
      stdgens = makeSeeds

  State
    { stateGame           = gs
    , stateSettings       = ogsettings
    , stateWParams        = genWParams
    , stateStdGens        = stdgens
    , stateZoom           = 120.0
    , stateGrid           = (take (gridw*gridh) (repeat 1))
    , stateOG             = (take (gridw*gridh) (repeat 1))
    , stateElev           = (take (gridw*gridh) (repeat 1))
    , stateCursor         = (5, 5)
    , stateTime           = 0
    }

-- this is used to generate the params data
genWParams :: [StdGen] -> WorldParams
genWParams sgs = WorldParams { nconts = nconts0
                             , conts  = conts0
                             , seeds  = seeds0
                             , rands  = rands0
                             , sizes  = sizes0
                             , types  = types0
                             , randi  = randi0
                             , rrands = rrands0 }
  where nconts0   = withStdGen sgs 1 $ randomRs (minnconts, maxnconts)
        conts0    = tail $ buildList2 ((withStdGen sgs 1 (randomList (fudge, (gridw-fudge)) nconts)), (withStdGen sgs 2 (randomList (1, (gridh-1)) nconts0)
        seeds0    = tail $ makeSeeds seedsseed nspots sgs
        seedsseed = buildList2 ((withStdGen sgs 3 (randomList (fudge, (gridw-fudge)) nconts0)), (withStdGen sgs 4 (randomList (1, (gridh-1)) nconts)))
        rands0    = tail $ makeSeeds randsseed nspots (splitSGs sgs)
        randsseed = buildList2 ((withStdGen sgs 5 (randomList (fudge, (gridw-fudge)) nconts0)), (withStdGen sgs 6 (randomList (1, (gridh-1)) nconts)))
        sizes0    = withStdGen sgs 1 $ randomList (minsize, maxsize) nconts
        types0    = withStdGen sgs 2 $ randomList (minsize, maxsize) nconts
        randi0    = withStdGen sgs


-- helps genereate parameters for world builder
makeSeeds :: [(Int, Int)] -> [Int] -> [StdGen] -> [[(Int, Int)]]
makeSeeds []          _              _  = []
makeSeeds ((x,y):xys) (nspot:nspots) sgs = (buildList2 (withStdGen sgs 1 (randomList ((x-fudge), (x+fudge)) (nspot))), (withStdGen sgs 2 (randomList ((y-fudge), (x+fudge)) (nspot)))):(makeSeeds xys (splitSGs sgs))

