module Main where
-- all the magic happens...

import System.Exit
import System.IO

import qualified GLUtil.ABFA as GLFW
import ABFA.Game
import ABFA.Event
import ABFA.Settings
import ABFA.State

main :: IO ()
main = do
  -- this will import screen width and height from lua script
  settings <- importSettings
  let state   = initState SMenu settings
      fs = fullscreen settings
      sw = screenw settings
      sh = screenh settings
  --event channel handles user input, state changes, and loading screens
  eventsChan <- newQueue --newTQueueIO :: IO (TQueue Event)

  -- opens the GLFW and sets the callbacks to handle errors and user input
  GLFW.withWindow fs sw sh "A Bridge Far Away..." $ \window -> do
    GLFW.setErrorCallback              $ Just $ errorCallback       eventsChan
    GLFW.setKeyCallback         window $ Just $ keyCallback         eventsChan
    GLFW.setMouseButtonCallback window $ Just $ mouseButtonCallback eventsChan
    GLFW.setWindowSizeCallback  window $ Just $ reshapeCallback     eventsChan
    GLFW.setScrollCallback      window $ Just $ scrollCallback      eventsChan
    GLFW.swapInterval 0

  print $ "hello..." ++ (show settings)
    -- runs the whole monad
    --void $ evalRWST run env state

