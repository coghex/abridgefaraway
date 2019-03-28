module ABFA.State where
import ABFA.Settings

-- the state is defined

-- the game state consists of most everything time critical
-- that happens, such as window state, random seeds, screen
-- width and height, grids and zones.
data State = State
  { stateGame     :: !GameState -- the current screen
  , stateSettings :: !Settings  -- the currents settings
  } deriving (Eq, Show)

-- the gamestate controls which screen we are currently drawing
data GameState = SMenu | SLoadWorld | SLoadElev | SWorld | SElev | SPause | SFucked deriving (Eq, Show)

-- initializes the state
initState :: GameState -> Settings -> State
initState gs settings = State { stateGame = gs
                              , stateSettings = settings }
