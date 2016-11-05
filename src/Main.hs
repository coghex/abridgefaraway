module Main (main) where

import Control.Monad (when, void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.RWS.Strict (RWST, liftIO, asks, ask, get, evalRWST, modify)
import Control.Concurrent (setNumCapabilities, threadDelay)
import Control.Concurrent.STM (TQueue, newTQueueIO, atomically, writeTQueue, tryReadTQueue)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Random (newStdGen)
import Graphics.Rendering.FTGL
import qualified GHC.Conc (getNumProcessors)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW

import World
import Util
import State
import Draw
import Rand
import Settings

-- Game type
type Game = RWST Env () State IO

-- runs game on every core
runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities

-- main function
main :: IO ()
main = do
  -- IO Queue
  eventsChan <- newTQueueIO :: IO (TQueue Event)
  runOnAllCores
  withWindow screenw screenh "A Bridge Far Away..." $ \window -> do
    GLFW.setErrorCallback        $ Just $ errorCallback eventsChan
    GLFW.setKeyCallback   window $ Just $ keyCallback   eventsChan
    GLFW.swapInterval 0
    texs <- liftIO $ initTexs window
    seed <- newStdGen
    s1 <- newStdGen
    s2 <- newStdGen
    continents <- randomN 10 25
    nzazzs <- randomN 10 maxnzazzs
    nspotscont <- randomN 20 maxnspots



    let nspots = randomList (25, 100::Int) continents seed
    let l = (take (gridh*gridw) (repeat seatile))
    let contl1 = buildList2 ((randomList (fudge, (gridw-fudge)) continents seed), (randomList (fudge, (gridh-fudge)) continents seed)) 
    let contl2 = buildList2 ((randomList ((fudge+1), (gridw-fudge-1)) continents seed), (randomList ((fudge+1), (gridh-fudge-1)) continents seed)) 
    let contl3 = buildList2 ((randomList ((fudge+2), (gridw-fudge-2)) continents seed), (randomList ((fudge+2), (gridh-fudge-2)) continents seed)) 
    let contl4 = buildList2 ((randomList ((0), (fudge)) nspotscont seed), (randomList ((0), (fudge-1)) nspotscont seed)) 
    let contl5 = buildList2 ((randomList ((0), (fudge-5)) nices seed), (randomList ((0), (fudge-6)) nices seed)) 
    let contl6 = buildList2 ((randomList ((2), (gridw-2)) nices seed), (randomList ((0), (fudge-4)) nices seed)) 
    let contl7 = buildList2 ((randomList ((1), (gridw-1)) nices seed), (randomList ((1), (fudge-4)) nices seed)) 
    let contl8 = buildList2 ((randomList ((0), (fudge-7)) nices seed), (randomList ((0), (fudge-6)) nices seed)) 
    let contl9 = buildList2 ((randomList ((2), (gridw-2)) nices seed), (randomList ((gridh-6), (gridh)) nices seed)) 
    let contl10 = buildList2 ((randomList ((1), (gridw-1)) nices seed), (randomList ((gridh-5), (gridh-1)) nices seed)) 
    let contl11 = buildList2 ((randomList ((0), (gridw)) nzazzs seed), (randomList ((0), (gridh)) nzazzs seed)) 
    let contl12 = buildList2 ((randomList ((2), (gridw-1)) nzazzs seed), (randomList ((2), (gridh)) nzazzs seed)) 
    let contl13 = buildList2 ((randomList ((1), (gridw-2)) nzazzs seed), (randomList ((1), (gridh-1)) nzazzs seed)) 
    let contl14 = (randomList ((1), (ntiles)) nzazzs seed)

    let env = Env
            { envEventsChan  = eventsChan
            , envWindow      = window
            , envGridWidth   = gridw
            , envGridHeight  = gridh
            }
        state = State
            { stateGrid      = l
            , stateTexs      = texs
            , stateGame      = SWorld
            , stateConts     = contl1
            , stateSeeds     = makeSeeds contl2 0 s1 s2 nspots
            , stateRands     = makeSeeds contl3 (2*continents) s1 s2 nspots
            , stateTileSizes = (randomList (0, maxcontsize) (gridw*gridh+fudge) seed)
            , stateTileRands = (randomList (0, ntiles) (continents+1) seed)
            , stateContSizes = contl4
            , stateSIceSizes = contl5
            , stateSIces     = contl6
            , stateSIceRands = contl7
            , stateNIceSizes = contl8
            , stateNIces     = contl9
            , stateNIceRands = contl10
            , stateZazzs     = contl11
            , stateZazzSizes = contl12
            , stateZazzRands = contl13
            , stateZazzTypes = contl14
            }
    let state2 = buildGrid state continents
    let state3 = zazzGrid state2
    let state4 = iceGrid state3
    void $ evalRWST run env state4

-- GLloop
run :: Game ()
run = timedLoop $ \lastTick tick -> do
  state <- get
  draw (stateGame state)
  window <- asks envWindow
  liftIO $ do
    GLFW.swapBuffers window
    GLFW.pollEvents
    err <- GL.get GL.errors
    mapM_ print err
    whileM_ ((\cur -> (cur - tick) < (1.0 / 60.0)) <$> getCurTick) (threadDelay 100)
  processEvents
  liftIO $ not <$> GLFW.windowShouldClose window


draw :: GameState -> Game ()
draw SWorld = do
  env   <- ask
  state <- get
  liftIO $ do
    GL.clear [GL.ColorBuffer]
    sceneSetup
    --drawTile (stateTexs state) 1 1 1
    --drawTile (stateTexs state) 2 2 1
    --drawTile (stateTexs state) 3 3 1
    drawScene state (envWindow env)
    GL.flush
draw _     = liftIO $ do
    GL.clear [GL.ColorBuffer]
    font <- createTextureFont "data/fonts/amatic/AmaticSC-Regular.ttf"
    setFontFaceSize font 24 72
    renderFont font "hello" Front
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
        state <- get
        when (k == GLFW.Key'Escape) $
          liftIO $ GLFW.setWindowShouldClose window True
        when ((k == GLFW.Key'Space) && ((stateGame state) == SWorld)) $
          modify $ \s -> s { stateGame = SMenu }
        when ((k == GLFW.Key'Space) && ((stateGame state) == SMenu)) $
          modify $ \s -> s { stateGame = SWorld }


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
