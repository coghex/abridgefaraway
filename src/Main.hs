module Main where

import Control.Monad (unless, when, void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.RWS.Strict (RWST, liftIO, asks, ask, get, evalRWST, modify, local)
import Control.Concurrent (setNumCapabilities, threadDelay, forkIO)
import Control.Concurrent.STM (TQueue, newTQueueIO, atomically, writeTQueue, tryReadTQueue)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, tryReadTChan, writeTChan, dupTChan, isEmptyTChan)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Random (newStdGen, mkStdGen, randomRs)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW
import qualified GHC.Conc (getNumProcessors)

import System.Exit
import System.IO

import Game.State
import Game.Font
import Game.Settings
import Game.Util
import Game.Draw
import Game.World
import Game.Elev
import Game.Rand
import Game.Time
import Game.Sun

type Game = RWST Env () State IO

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes
    
main :: IO ()
main = do
  runOnAllCores
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  withWindow screenw screenh "A Bridge Far Away..." $ \window -> do
    initWindow
    GLFW.setErrorCallback             $ Just $ errorCallback   eventsChan
    GLFW.setKeyCallback        window $ Just $ keyCallback     eventsChan
    GLFW.setWindowSizeCallback window $ Just $ reshapeCallback eventsChan
    GLFW.swapInterval 0

    f1 <- loadFont "data/fonts/amatic/AmaticSC-Regular.ttf"
    f2 <- loadFont "data/fonts/amatic/AmaticSC-Regular.ttf"
    (wtex, ztex) <- liftIO $ initTexs window

    s1 <- newStdGen
    s2 <- newStdGen
    s3 <- newStdGen
    s4 <- newStdGen
    s5 <- newStdGen
    s6 <- newStdGen
    
    let nconts = (randomRs (minnconts, maxnconts) (mkStdGen 42)) !! 1
    let rangers = (randomRs (minnconts, maxnconts) (mkStdGen 43))

    let sol = (makeSun 0.0 ((fromIntegral(gridh))/2) 800 60)
        state = genParams SMenu 1 nconts 0 rangers sol s1 s2 s3 s4 s5 s6
    stateChan1 <- atomically $ newTChan
    stateChan2 <- atomically $ newTChan
    timerChan  <- atomically $ newTChan
    let env    = Env
            { envEventsChan = eventsChan
            , envWindow     = window
            , envFontBig    = f1
            , envFontSmall  = f2
            , envWTex       = wtex
            , envZTex       = ztex
            , envSeeds      = randomRs (1,100) (mkStdGen 43)
            , envStateChan1 = stateChan1
            , envStateChan2 = stateChan2
            , envTimerChan  = timerChan
            }


    forkIO $ (gameTime env state 100 TStop)
    void $ evalRWST run env state

run :: Game ()
run = do
  env <- ask
  timedLoop $ \lastTick tick -> do
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
    drawText (envFontBig env) 1 95 72 72 "A Bridge Far Away..."
    drawText (envFontSmall env) 1 75 36 36 "press c to create a world"
    drawText (envFontSmall env) 1 60 36 36 "press l to load a world"
    drawText (envFontSmall env) 1 45 36 36 "press esc to quit"
draw SLoad = do
  env   <- ask
  state <- get
  liftIO $ do
    beginDrawText
    drawText (envFontBig env) 1 95 72 72 "Loading..."
    drawText (envFontSmall env) 1 75 36 36 "Creating World..."
    atomically $ writeTChan (envTimerChan env) TStart
    atomically $ readTChan (envStateChan1 env)
    liftIO $ loadedCallback (envEventsChan env) SWorld
draw SLoadElev = do
  env   <- ask
  state <- get
  liftIO $ do
    beginDrawText
    drawText (envFontBig env) 1 95 72 72 "Loading Elevation..."
    liftIO $ loadedCallback (envEventsChan env) SElev
draw SWorld = do
  env   <- ask
  state <- get
  liftIO $ do
    GL.clear[GL.ColorBuffer, GL.DepthBuffer]
    statebuff <- atomically $ tryReadTChan (envStateChan1 env)
    newstate <- case (statebuff) of
      Nothing -> return state
      Just n  -> return n
    let unftime = stateTime newstate
    beginDrawText
    drawText (envFontSmall env) (-120) (-40) 36 36 $ formatTime unftime
    GL.preservingMatrix $ do
      drawScene newstate (envWTex env)
    GL.preservingMatrix $ do
      drawCursor newstate (envWTex env)
    liftIO $ timerCallback (envEventsChan env) newstate
draw SElev = do
  env   <- ask
  state <- get
  liftIO $ do
    GL.clear[GL.ColorBuffer, GL.DepthBuffer]
    statebuff <- atomically $ tryReadTChan (envStateChan1 env)
    newstate <- case (statebuff) of
      Nothing -> return state
      Just n  -> return n
    let unftime = stateTime newstate
        sun     = stateSun newstate
    beginDrawText
    drawText (envFontSmall env) (-120) (-40) 36 36 $ formatTime unftime
    drawText (envFontSmall env) (-120) (-25) 36 36 $ "x:" ++ (show (fst (stateCursor newstate))) ++ " y:" ++ (show (snd (stateCursor newstate)))
    drawText (envFontSmall env) (-120) (-10) 36 36 $ formatElev (stateElev newstate) (stateCursor newstate)
    GL.preservingMatrix $ do
      drawElev newstate (envWTex env)
    GL.preservingMatrix $ do
      drawCursor newstate (envWTex env)
    liftIO $ timerCallback (envEventsChan env) newstate
draw _ = do
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
        when (((stateGame state) == SMenu) && (k == GLFW.Key'Escape)) $ do
            liftIO $ GLFW.setWindowShouldClose window True
        when (((stateGame state) == SMenu) && (k == GLFW.Key'C)) $ do
            liftIO $ loadedCallback (envEventsChan env) SLoad
            let newstate = initWorld state env
            liftIO $ atomically $ writeTChan (envStateChan2 env) newstate
            modify $ \s -> s { stateGrid = (stateGrid newstate)
                             , stateElev = (stateElev newstate)
                             }
        when (((stateGame state) == SWorld) && (k == GLFW.Key'R)) $ do
            liftIO $ atomically $ writeTChan (envTimerChan env) TStop
            liftIO $ emptyChan (envStateChan1 env)
            liftIO $ emptyChan (envStateChan2 env)
            liftIO $ loadedCallback (envEventsChan env) SLoad
            let newstate = regenWorld state env
            liftIO $ emptyChan (envStateChan1 env)
            liftIO $ emptyChan (envStateChan2 env)
            liftIO $ atomically $ writeTChan (envStateChan2 env) newstate
            modify $ \s -> newstate
        when (((stateGame state) == SWorld) && (k == GLFW.Key'E)) $ do
            modify $ \s -> s { stateGame = SLoadElev }
        when (((stateGame state) == SWorld) && (k == GLFW.Key'Left)) $ do
            modify $ \s -> s { stateCursor = (((fst (stateCursor state))-1), (snd (stateCursor state))) }
        when (((stateGame state) == SWorld) && (k == GLFW.Key'Right)) $ do
            modify $ \s -> s { stateCursor = (((fst (stateCursor state))+1), (snd (stateCursor state))) }
        when (((stateGame state) == SWorld) && (k == GLFW.Key'Up)) $ do
            modify $ \s -> s { stateCursor = ((fst (stateCursor state)), ((snd (stateCursor state))+1)) }
        when (((stateGame state) == SWorld) && (k == GLFW.Key'Down)) $ do
            modify $ \s -> s { stateCursor = ((fst (stateCursor state)), ((snd (stateCursor state))-1)) }
        when (((stateGame state) == SElev) && (k == GLFW.Key'Left)) $ do
            modify $ \s -> s { stateCursor = (((fst (stateCursor state))-1), (snd (stateCursor state))) }
        when (((stateGame state) == SElev) && (k == GLFW.Key'Right)) $ do
            modify $ \s -> s { stateCursor = (((fst (stateCursor state))+1), (snd (stateCursor state))) }
        when (((stateGame state) == SElev) && (k == GLFW.Key'Up)) $ do
            modify $ \s -> s { stateCursor = ((fst (stateCursor state)), ((snd (stateCursor state))+1)) }
        when (((stateGame state) == SElev) && (k == GLFW.Key'Down)) $ do
            modify $ \s -> s { stateCursor = ((fst (stateCursor state)), ((snd (stateCursor state))-1)) }
        when (((stateGame state) == SElev) && ((k == GLFW.Key'E) || (k == GLFW.Key'Escape))) $ do
            modify $ \s -> s { stateGame = SWorld }
        when (((stateGame state) == SWorld) && (k == GLFW.Key'Escape)) $ do
            liftIO $ GLFW.setWindowShouldClose window True
    (EventFramebufferSize _ width height) -> do
      adjustWindow
    (EventWindowResize win w h) -> do
      adjustWindow
    (EventLoaded state) -> do
      modify $ \s -> s { stateGame = state }
    (EventUpdateState state) -> do
      modify $ \s -> s { stateGrid     = (stateGrid state)
                       , stateElev     = (stateElev state)
                       , stateSun      = (stateSun state)
                       , stateSunSpots = (stateSunSpots state)
                       , stateTime     = (stateTime state)
                       , stateOceans   = (stateOceans state)
                       , stateSkies    = (stateSkies state)
                       }

emptyChan :: TChan a -> IO ()
emptyChan chan = do
  test <- (atomically (isEmptyTChan chan))
  if test then return ()
  else do
    s <- atomically $ readTChan chan
    emptyChan chan

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

errorCallback :: TQueue Event -> GLFW.Error -> String -> IO ()
errorCallback tc e s = atomically $ writeTQueue tc $ EventError e s
keyCallback :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey win k sc ka mk
reshapeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
reshapeCallback tc win w h = atomically $ writeTQueue tc $ EventWindowResize win w h
loadedCallback :: TQueue Event -> GameState -> IO ()
loadedCallback tc state = atomically $ writeTQueue tc $ EventLoaded state
timerCallback :: TQueue Event -> State -> IO ()
timerCallback tc state = atomically $ writeTQueue tc $ EventUpdateState state
