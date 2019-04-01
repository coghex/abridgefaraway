module ABFA.Game where
-- all of the RWST monadic operations are defined here,
-- including the state and env...

import Control.Monad.Trans (MonadIO)
import Control.Monad.RWS.Strict (RWST, liftIO, asks, ask, gets, get, evalRWST, modify, local)

import qualified GLUtil.ABFA as GLFW
import GLUtil.Font
import ABFA.Event
import ABFA.State

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
  , envFonts      :: ![Font]      -- the list of fonts loaded
  }
