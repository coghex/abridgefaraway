module Main where

import Control.Monad (when, void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.RWS.Strict (RWST, liftIO, asks, ask, get, evalRWST, modify, local)
import Control.Concurrent (setNumCapabilities, threadDelay)
import Control.Concurrent.STM (TQueue, newTQueueIO, atomically, writeTQueue, tryReadTQueue)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Random (newStdGen)
import Graphics.Rendering.FTGL
import qualified GHC.Conc (getNumProcessors)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW

import Game.Rand
import Game.Font
import Game.Settings
import Game.State
import Game.Util
import Game.World
import Game.Draw
import Game.Save

type Game = RWST Env () State IO

main :: IO ()
main = do
  eventsChan <- newTQueueIO :: IO (TQueue Event)
  runOnAllCores
  withWindow screenw screenh "A Bridge Far Away..." $ \window -> do
    initWindow
    GLFW.setErrorCallback             $ Just $ errorCallback   eventsChan
    GLFW.setKeyCallback        window $ Just $ keyCallback     eventsChan
    GLFW.setWindowSizeCallback window $ Just $ reshapeCallback eventsChan
    GLFW.swapInterval 0
    f1 <- loadFont "data/fonts/amatic/AmaticSC-Regular.ttf"
    f2 <- loadFont "data/fonts/amatic/AmaticSC-Regular.ttf"
    wtexs <- liftIO $ initTexs window
    s1 <- newStdGen
    s2 <- newStdGen
    s3 <- newStdGen
    s4 <- newStdGen
    s5 <- newStdGen
    s6 <- newStdGen
    nconts <- randomN minnconts maxnconts
    inspots <- randomN mininspots maxinspots
    zspots <- randomN minzspots maxzspots
    let nspots = randomList (minnspots, maxnspots) nconts s1
        conts = buildList2 ((randomList ((fudge), (gridw-fudge)) nconts s1), (randomList (fudge, (gridh-fudge)) nconts s2))
        seeds = buildList2 ((randomList ((fudge+1), (gridw-fudge-1)) nconts s3), (randomList ((fudge+1), (gridh-fudge-1)) nconts s4))
        rands = buildList2 ((randomList ((fudge+2), (gridw-fudge-2)) nconts s5), (randomList ((fudge+2), (gridh-fudge-2)) nconts s5))
        sizes = randomList (minsize, maxsize) nconts s1
        types = randomList (0, 4::Int) nconts s2
        siconts = buildList2 ((randomList (0, (gridw)) inspots s1), (randomList(0, 10::Int) inspots s2))
        sisizes = buildList2 ((randomList (minicesize, maxicesize) inspots s2), (randomList (minicesize, maxicesize) inspots s3))
        sirands = buildList2 ((randomList (1, (gridw-1)) inspots s5), (randomList (1, 9::Int) inspots s6))
        niconts = buildList2 ((randomList (0, gridw) inspots s2), (randomList ((gridh-fudge), gridh) inspots s1))
        nisizes = buildList2 ((randomList (minicesize, maxicesize) inspots s5), (randomList (minicesize, maxicesize) inspots s6))
        nirands = buildList2 ((randomList (1, (gridw-1)) inspots s3), (randomList ((gridh-fudge+1), (gridh-1)) inspots s4))
        zs = buildList2 ((randomList (0, (gridw)) zspots s6), (randomList(0, gridh) zspots s5))
        zss = buildList2 ((randomList (minzazzsize, maxzazzsize) zspots s2), (randomList (minzazzsize, maxzazzsize) zspots s3))
        zrs = buildList2 ((randomList (1, (gridw-1)) zspots s2), (randomList (1, (gridh-1)) zspots s1))
        ztr = randomList (0, 5::Int) zspots s1
        
        
    let env = Env
            { envEventsChan   = eventsChan
            , envWindow       = window
            , envFontBig      = f1
            , envFontSmall    = f2
            , envWTexs        = wtexs
            , envSeeds        = [s1, s2, s3, s4, s5, s6]
            }
        state = State
            { stateGame       = SMenu
            , stateScreenW    = screenw
            , stateScreenH    = screenh
            , stateGrid       = (take (gridw*gridh) (repeat 4))
            , stateNConts     = nconts
            , stateISpots     = inspots
            , stateZSpots     = zspots
            , stateNSpots     = nspots
            , stateConts      = tail conts
            , stateSeeds      = tail $ makeSeeds seeds 0 s1 s2 nspots
            , stateRands      = tail $ makeSeeds rands (2*nconts) s3 s4 nspots
            , stateSizes      = sizes
            , stateTypes      = types
            , stateSIceConts  = siconts
            , stateSIceSizes  = sisizes
            , stateSIceRands  = sirands
            , stateNIceConts  = niconts
            , stateNIceSizes  = nisizes
            , stateNIceRands  = nirands
            , stateZazzConts  = zs
            , stateZazzSizes  = zss
            , stateZazzRands  = zrs
            , stateZazzTypes  = ztr
            }
    --let newstate = initWorld state (nconts-2)
    void $ evalRWST run env state

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
    whileM_ ((\cur -> (cur - tick) < (1.0/60.0)) <$> getCurTick) (threadDelay 100)
  processEvents
  liftIO $ not <$> GLFW.windowShouldClose window

draw :: GameState -> Game ()
draw SMenu = do
  env   <- ask
  state <- get
  liftIO $ do
    beginDrawText
    drawText (envFontBig env) 1 95 24 72 "A Bridge Far Away..."
    drawText (envFontSmall env) 1 75 12 36 "press c to create a world"
    drawText (envFontSmall env) 1 65 12 36 "press l to load a world"
draw SLoad = do
  env   <- ask
  state <- get
  liftIO $ do
    beginDrawText
    drawText (envFontBig env) 1 95 24 72 "Loading..."
    liftIO $ loadedCallback (envEventsChan env)
draw SWorld = do
  env   <- ask
  state <- get
  liftIO $ do
    GL.clear[GL.ColorBuffer, GL.DepthBuffer]
    GL.preservingMatrix $ do
      drawScene state (envWTexs env)
draw _     = do
  state <- get
  liftIO $ do
    print "fuck"

runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities

timedLoop :: MonadIO m => (Double -> Double -> m Bool) -> m ()
timedLoop f = loop 0.0
  where
    loop lastTick = do
      tick <- liftIO getCurTick
      quit <- f lastTick tick
      when quit $ loop tick

whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ p f = do
  x <- p
  when x $ do f >> whileM_ p f

getCurTick :: IO Double
getCurTick = do
  tickUCT <- getCurrentTime
  return (fromIntegral (round $ utctDayTime tickUCT * 1000000 :: Integer) / 1000000.0 :: Double)

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
        env   <- ask
        when (k == GLFW.Key'Escape) $ do
          liftIO $ saveMap (stateGrid state)
          liftIO $ GLFW.setWindowShouldClose window True
        when (k == GLFW.Key'Space) $ do
          modify $ \s -> s { stateGame = SMenu }
        when (k == GLFW.Key'C) $ do
          modify $ \s -> s { stateGame = SLoad }
          let newstate = initWorld state (length (stateConts state))
          modify $ \s -> s { stateGrid = (stateGrid newstate) }
        when ((k == GLFW.Key'L) && ((stateGame state) == SMenu)) $ do
          x <- liftIO loadMap
          modify $ \s -> s { stateGrid = x
                           , stateGame = SWorld }
    (EventFramebufferSize _ width height) -> do
      adjustWindow
    (EventWindowResize win w h) -> do
      adjustWindow
    (EventLoaded) -> do
      modify $ \s -> s { stateGame = SWorld }

adjustWindow :: Game ()
adjustWindow = do
  state <- get
  let width  = screenw
      height = screenh
  let pos    = GL.Position 0 0
      size   = GL.Size (fromIntegral width) (fromIntegral height)
      h      = fromIntegral height / fromIntegral width :: Double
  liftIO $ do
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GLU.perspective 45 (1/h) (0.1) 500
    GL.matrixMode GL.$= GL.Modelview 0
    GL.loadIdentity

initWindow :: IO ()
initWindow = do
  let width  = screenw
      height = screenh
  let pos    = GL.Position 0 0
      size   = GL.Size (fromIntegral width) (fromIntegral height)
      h      = fromIntegral height / fromIntegral width :: Double
  do
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GLU.perspective 45 (1/h) (0.1) 500
    GL.matrixMode GL.$= GL.Modelview 0
    GL.loadIdentity

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h title f = do
  GLFW.setErrorCallback $ Just glfwErrorCallback
  True <- GLFW.init
  Just window <- GLFW.createWindow w h title Nothing Nothing
  GLFW.makeContextCurrent $ Just window
  f window
  GLFW.setErrorCallback $ Just glfwErrorCallback
  GLFW.destroyWindow window
  GLFW.terminate
  where
    glfwErrorCallback e s = putStrLn $ show e ++ " " ++ show s

errorCallback   :: TQueue Event -> GLFW.Error -> String -> IO ()
errorCallback tc e s = atomically $ writeTQueue tc $ EventError e s
keyCallback     :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey win k sc ka mk
reshapeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
reshapeCallback tc win w h = atomically $ writeTQueue tc $ EventWindowResize win w h
loadedCallback :: TQueue Event -> IO ()
loadedCallback tc = atomically $ writeTQueue tc $ EventLoaded


