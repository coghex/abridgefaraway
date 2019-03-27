module ABFA.Game where
-- all of the RWST monadic operations are defined here,
-- including the state and env...

import Control.Monad.Trans (MonadIO)
import Control.Monad.RWS.Strict (RWST, liftIO, asks, ask, gets, get, evalRWST, modify, local)

import qualified GLUtil.ABFA as GLFW
import ABFA.Event

-- the game monad wrapper, providing a threadsafe state and env
type Game = RWST Env () State IO

-- the game enviornment mainly consisting of the GLFW window,
-- the event queue, the loaded textures, and the various
-- channels to communicate with threads
data Env = Env
  { envEventsChan :: Queue Event  -- the events channel that all
                                  -- user interface and much thread
                                  -- communication occurs in
  , envWindow     :: !GLFW.Window -- the window defined by GLFW
  }

-- the game state consists of most everything time critical
-- that happens, such as window state, random seeds, screen
-- width and height, grids and zones.
data State = State
  { stateGame    :: !GameState
  }

-- the gamestate controls which screen we are currently drawing
data GameState = SMenu | SLoadWorld | SLoadElev | SWorld | SElev | SPause | SFucked deriving (Eq, Show)
