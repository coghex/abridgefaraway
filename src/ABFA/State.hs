module ABFA.State where
-- the state is defined

-- the game state consists of most everything time critical
-- that happens, such as window state, random seeds, screen
-- width and height, grids and zones.
data State = State
  { stateGame    :: !GameState
  } deriving (Eq, Show)

-- the gamestate controls which screen we are currently drawing
data GameState = SMenu | SLoadWorld | SLoadElev | SWorld | SElev | SPause | SFucked deriving (Eq, Show)
