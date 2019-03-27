module Main where

import Control.Monad (unless, when, void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.RWS.Strict (RWST, liftIO, asks, ask, get, evalRWST, modify, local)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (rpar, parMap)
import Control.Concurrent (setNumCapabilities, threadDelay, forkIO, forkOS)
import Control.Concurrent.STM (TQueue, newTQueueIO, atomically, writeTQueue, tryReadTQueue)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, tryReadTChan, writeTChan, dupTChan, isEmptyTChan)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Random (newStdGen, mkStdGen, randomRs)
--import Foreign.Lua

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW
import qualified GHC.Conc (getNumProcessors)

import System.Exit
import System.IO

main :: IO ()
main = do
  -- event channel handles user input, state changes, and loading screens
  --eventsChan <- newTQueueIO :: IO (TQueue Event)

  -- opens the GLFW and sets the callbacks to handle errors and user input
  --withWindow screenw screenh "A Bridge Far Away..." $ \window -> do
  --  initWindow
  --  GLFW.setErrorCallback              $ Just $ errorCallback       eventsChan
  --  GLFW.setKeyCallback         window $ Just $ keyCallback         eventsChan
  --  GLFW.setMouseButtonCallback window $ Just $ mouseButtonCallback eventsChan
  --  GLFW.setWindowSizeCallback  window $ Just $ reshapeCallback     eventsChan
  --  GLFW.setScrollCallback      window $ Just $ scrollCallback      eventsChan
  --  GLFW.swapInterval 0
  print "hello..."
    -- runs the whole monad
    --void $ evalRWST run env state
