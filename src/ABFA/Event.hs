module ABFA.Event where
-- the event queue that handles all user input and many
-- other things is defined

import Control.Concurrent.STM (TQueue, newTQueueIO)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar

import qualified GLUtil.ABFA as GLFW

type Queue = TQueue

data Event =
    EventError          !GLFW.Error !String
  | EventWindowPos      !GLFW.Window !Int !Int
  deriving Show

newQueue :: IO (TQueue Event)
newQueue = newTQueueIO
