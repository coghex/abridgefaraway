module ABFA.State where
-- the state is defined

import System.Random
import ABFA.Settings
import ABFA.Data


-- the game state consists of most everything time critical
-- that happens, such as window state, random seeds, screen
-- width and height, grids and zones.
data State = State
  { stateGame      :: !GameState   -- the current screen
  , stateGamePrev  :: !GameState   -- the previous screen
  , stateSettings  :: !Settings    -- the currents settings
  , stateWParams   :: !WorldParams -- parameters for the world generator
  , stateStdGens   :: ![StdGen]    -- a set of rngs
  , stateZoom      :: !Float       -- the zoom when in the zone screen
  , stateGrid      :: ![Int]       -- the world grid
  , stateOG        :: ![Int]       -- the original world grid
  , stateElev      :: ![Int]       -- the average elevation of each tile
  , stateCursor    :: !(Int, Int)  -- the cursor position
  , stateTime      :: !Integer     -- the time since 0
  , stateShellBuff :: ![String]    -- a string containing the shell history and prompt
  }

-- the gamestate controls which screen we are currently drawing
data GameState = SMenu | SShell | SLoadWorld | SLoadTime | SLoadElev | SWorld | SElev | SPause | SFucked deriving (Eq, Show)

-- initializes the state
initState :: GameState -> [StdGen] -> Settings -> State
initState gs seeds settings = State { stateGame      = gs
                                    , stateGamePrev  = SFucked
                                    , stateSettings  = settings
                                    , stateWParams   = emptyParams
                                    , stateStdGens   = seeds
                                    , stateZoom      = 1
                                    , stateGrid      = []
                                    , stateOG        = []
                                    , stateElev      = []
                                    , stateCursor    = (5, 5)
                                    , stateTime      = 0
                                    , stateShellBuff = [] }


emptyParams :: WorldParams
emptyParams = WorldParams { wpNConts = 0
                          , wpConts  = []
                          , wpSeeds  = []
                          , wpRands  = []
                          , wpSizes  = []
                          , wpTypes  = []
                          , wpRandI  = 0
                          , wpRRands = []
                          }
