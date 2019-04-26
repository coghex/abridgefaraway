module ABFA.Input where
-- the inputs from mouse and keyboard are processed

import Control.Monad.RWS.Strict (liftIO, asks, ask, gets, get, modify)
import Control.Monad (when)
import qualified GLUtil.ABFA as GLFW
import ABFA.Game
import ABFA.State
import ABFA.Data
import ABFA.Event
import ABFA.World

-- case function for all of the keys
evalKey :: GLFW.Window -> GLFW.Key -> Game ()
evalKey window k = do
  state <- get
  env   <- ask
  let keylayout = settingKeyLayout (stateSettings state)
  let gs        = stateGame state
  -- quits from the menu
  when ((gs == SMenu) && (keyCheck keylayout k "ESC")) $ do
    liftIO $ GLFW.closeGLFW window
  -- exits when in world view, in future should save game
  when ((gs == SWorld) && (keyCheck keylayout k "ESC")) $ do
    liftIO $ GLFW.closeGLFW window
  -- creates world from the menu
  when ((gs == SMenu) && (keyCheck keylayout k "C")) $ do
    liftIO $ loadedCallback (envEventsChan env) SLoadWorld
    let newstate = initGoodWorld state
    -- let the timers know that we have generated a new state
    liftIO $ atomically $ writeChan (envStateChan2 env) newstate
    liftIO $ atomically $ writeChan (envStateChan4 env) newstate
    modify $ \s -> newstate
  -- regenerates the world from the world screen
  when ((gs == SWorld) && (keyCheck keylayout k "R")) $ do
    -- stop the timers
    liftIO $ atomically $ writeChan (envWTimerChan env) TStop
    liftIO $ atomically $ writeChan (envATimerChan env) TStop
    -- ensure empty channels
    liftIO $ emptyChan (envStateChan1 env)
    liftIO $ emptyChan (envStateChan2 env)
    liftIO $ emptyChan (envStateChan3 env)
    liftIO $ emptyChan (envStateChan4 env)
    -- create new world
    let newstate = initWorldWithCheck state
    -- just to be sure, empty the channels again
    liftIO $ emptyChan (envStateChan1 env)
    liftIO $ emptyChan (envStateChan2 env)
    liftIO $ emptyChan (envStateChan3 env)
    liftIO $ emptyChan (envStateChan4 env)
    -- alert the timers of the new state
    liftIO $ atomically $ writeChan (envStateChan2 env) newstate
    liftIO $ atomically $ writeChan (envStateChan4 env) newstate
    modify $ \s -> newstate


-- checks key with settings
keyCheck :: KeyLayout -> GLFW.Key -> String -> Bool
keyCheck keylayout k str = (k == (GLFW.getGLFWKey nk))
  where nk = getKey keylayout str

-- retrieves user key setting
getKey :: KeyLayout -> String -> String
getKey keylayout "ESC" = keyESC keylayout
getKey keylayout "C"   = keyC   keylayout
getKey keylayout "R"   = keyR   keylayout
getKey keylayout _     = "NULL"

