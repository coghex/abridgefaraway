module Main where
-- all the magic happens...

import System.Exit
import System.IO

import qualified GLUtil.ABFA as GLFW
import ABFA.Game
import ABFA.Event

main :: IO ()
main = do
  let screenw = 2560
      screenh = 1440
  --event channel handles user input, state changes, and loading screens
  eventsChan <- newQueue --newTQueueIO :: IO (TQueue Event)

  -- opens the GLFW and sets the callbacks to handle errors and user input
  GLFW.withWindow screenw screenh "A Bridge Far Away..." $ \window -> do
    GLFW.initWindow
    GLFW.setErrorCallback              $ Just $ errorCallback       eventsChan
    GLFW.setKeyCallback         window $ Just $ keyCallback         eventsChan
    GLFW.setMouseButtonCallback window $ Just $ mouseButtonCallback eventsChan
    GLFW.setWindowSizeCallback  window $ Just $ reshapeCallback     eventsChan
    GLFW.setScrollCallback      window $ Just $ scrollCallback      eventsChan
    GLFW.swapInterval 0
  print "hello..."
    -- runs the whole monad
    --void $ evalRWST run env state

