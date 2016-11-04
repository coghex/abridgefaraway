module Main (main) where

import Control.Monad (when, void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.RWS.Strict (RWST, liftIO, asks, ask, get, evalRWST)
import Control.Concurrent (setNumCapabilities, threadDelay)
import Control.Concurrent.STM (TQueue, newTQueueIO, atomically, writeTQueue, tryReadTQueue)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import qualified GHC.Conc (getNumProcessors)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW

import World
import Util
import State
import Draw

-- Game type
type Game = RWST Env () State IO

-- runs game on every core
runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities

-- main function
main :: IO ()
main = do
  -- screen size and map size
  let screenw = 1600
      screenh = 1200
      gridw   = 120
      gridh   = 90

  -- IO Queue
  eventsChan <- newTQueueIO :: IO (TQueue Event)
  runOnAllCores
  withWindow screenw screenh "A Bridge Far Away..." $ \window -> do
    GLFW.setErrorCallback        $ Just $ errorCallback eventsChan
    GLFW.setKeyCallback   window $ Just $ keyCallback   eventsChan
    GLFW.swapInterval 0
    texs <- liftIO $ initTexs window
    let l = (take (90*120) (repeat 5))
    let env = Env
            { envEventsChan = eventsChan
            , envWindow     = window
            , envGridWidth  = gridw
            , envGridHeight = gridh
            }
        state = State
            { stateGrid     = l
            , stateTexs     = texs
            , stateGame     = SWorld
            , stateConts    = []
            , stateSeeds    = [[]]
            , stateRands    = [[]]
            }
    void $ evalRWST run env state

-- GLloop
run :: Game ()
run = timedLoop $ \lastTick tick -> do
  draw
  window <- asks envWindow
  liftIO $ do
    GLFW.swapBuffers window
    GLFW.pollEvents
    err <- GL.get GL.errors
    mapM_ print err
    whileM_ ((\cur -> (cur - tick) < (1.0 / 60.0)) <$> getCurTick) (threadDelay 100)
  processEvents
  liftIO $ not <$> GLFW.windowShouldClose window

draw :: Game ()
draw = do
  state <- get
  env   <- ask
  liftIO $ do
    GL.clear [GL.ColorBuffer]
    sceneSetup
    --drawTile (stateTexs state) 1 1 1
    --drawTile (stateTexs state) 2 2 1
    --drawTile (stateTexs state) 3 3 1
    drawScene state (envWindow env)
    GL.flush

-- thread loop function
timedLoop :: MonadIO m => (Double -> Double -> m Bool) -> m ()
timedLoop f = loop 0.0
  where
    loop lastTick = do
      tick <- liftIO getCurTick
      quit <- f lastTick tick
      when quit $ loop tick

-- while monad loop
whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ p f = do
  x <- p
  when x $ do f >> whileM_ p f

-- time tick function
getCurTick :: IO Double
getCurTick = do
  tickUCT <- getCurrentTime
  return (fromIntegral (round $ utctDayTime tickUCT * 1000000 :: Integer) / 1000000.0 :: Double)

-- process events
processEvents :: Game ()
processEvents = do
  tc <- asks envEventsChan
  me <- liftIO $ atomically $ tryReadTQueue tc
  case me of
    Just e -> do
      processEvent e
      processEvents
    Nothing -> return ()

processEvent :: Event -> Game ()
processEvent ev =
  case ev of
    (EventError e s) -> do
      window <- asks envWindow
      liftIO $ do
        putStrLn $ "error " ++ show e ++ " " ++ show s
        GLFW.setWindowShouldClose window True
    (EventKey window k _ ks _) ->
      when (ks == GLFW.KeyState'Pressed) $ do
        when (k == GLFW.Key'Escape) $
          liftIO $ GLFW.setWindowShouldClose window True

-- GLFW window opening
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h title f = do
  GLFW.setErrorCallback $ Just glfwErrorCallback
  True <- GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
  Just window <- GLFW.createWindow w h title Nothing Nothing
  GLFW.makeContextCurrent $ Just window
  f window
  -- i dont know why this is done twice
  GLFW.setErrorCallback $ Just glfwErrorCallback
  GLFW.destroyWindow window
  GLFW.terminate
  where
    glfwErrorCallback e s = putStrLn $ show e ++ " " ++ show s

-- callbacks
errorCallback :: TQueue Event -> GLFW.Error -> String -> IO ()
errorCallback tc e s            = atomically $ writeTQueue tc $ EventError e s
keyCallback   :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback   tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey win k sc ka mk
