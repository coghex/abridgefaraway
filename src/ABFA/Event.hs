module ABFA.Event where
-- the event queue that handles all user input and many
-- other things is defined

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import qualified Control.Concurrent as CC

import qualified GLUtil.ABFA as GLFW
import ABFA.State

-- type synonym
type Queue = STM.TQueue

-- all possible events
data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventFrameBufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  | EventWindowResize    !GLFW.Window !Int !Int
  | EventLoaded          !GameState
  | EventUpdateState     !State
  | EventAnimState       !State

-- function synonym
newQueue     :: IO (STM.TQueue Event)
newQueue     = STM.newTQueueIO
tryReadQueue :: STM.TQueue a -> STM.STM (Maybe a)
tryReadQueue = STM.tryReadTQueue
readQueue    :: STM.TQueue a -> STM.STM a
readQueue    = STM.readTQueue
writeQueue   :: STM.TQueue a -> a -> STM.STM ()
writeQueue   = STM.writeTQueue
tryReadChan  :: STM.TChan a -> STM.STM (Maybe a)
tryReadChan  = STM.tryReadTChan
newChan      :: IO (TChan a)
newChan      = atomically $ STM.newTChan
readChan     :: STM.TChan a -> STM.STM a
readChan     = STM.readTChan
writeChan    :: STM.TChan a -> a -> STM.STM ()
writeChan    = STM.writeTChan
atomically   :: STM.STM a -> IO a
atomically   = STM.atomically
threadDelay  :: Int -> IO ()
threadDelay  = CC.threadDelay


-- empties a channel
emptyChan :: STM.TChan a -> IO ()
emptyChan chan = do
  test <- atomically $ STM.isEmptyTChan chan
  if test then return ()
  else do
    s <- atomically $ STM.readTChan chan
    emptyChan chan

-- these event callbacks allow me to do things to GLFW or mine own program
-- registers errors
errorCallback :: Queue Event -> GLFW.Error -> String -> IO ()
errorCallback tc e s = atomically $ writeQueue tc $ EventError e s
-- registers key states on input
keyCallback :: Queue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc win k sc ka mk = atomically $ writeQueue tc $ EventKey win k sc ka mk
-- registers mouse buttons
mouseButtonCallback :: Queue Event -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback tc win mb mbs mk = atomically $ writeQueue tc $ EventMouseButton win mb mbs mk
-- registers mouse scrolling
scrollCallback :: Queue Event -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback tx win x y = atomically $ writeQueue tx $ EventScroll win x y
-- called when the window is resized
reshapeCallback :: Queue Event -> GLFW.Window -> Int -> Int -> IO ()
reshapeCallback tc win w h = atomically $ writeQueue tc $ EventWindowResize win w h
-- call to change the state
loadedCallback :: Queue Event -> GameState -> IO ()
loadedCallback tc state = atomically $ writeQueue tc $ EventLoaded state
-- changes parts of the state with the world timer
timerCallback :: Queue Event -> State -> IO ()
timerCallback tc state = atomically $ writeQueue tc $ EventUpdateState state
-- changes parts of the state with the animation timer
animCallback :: Queue Event -> State -> IO ()
animCallback tc state = atomically $ writeQueue tc $ EventAnimState state
