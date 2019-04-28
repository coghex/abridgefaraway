module Main where
-- all the magic happens...

import System.Exit
import System.IO
import Control.Monad (unless, when, void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.RWS.Strict (RWST, liftIO, asks, ask, get, gets, evalRWST, modify, local)
import Control.Concurrent (threadDelay, forkIO, forkOS)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Random (newStdGen, mkStdGen)
import qualified Foreign.Lua as Lua

import qualified GLUtil.ABFA as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import GLUtil.ABFA
import GLUtil.Font
import GLUtil.Util
import GLUtil.Draw
import GLUtil.UI
import ABFA.Game
import ABFA.Data
import ABFA.Event
import ABFA.Settings
import ABFA.Input
import ABFA.State
import ABFA.Time
import ABFA.Rand
import ABFA.World
import ABFA.Shell

main :: IO ()
main = do
  -- this will provide some seeds to have consistent randomness
  seeds <- newSeeds
  -- this will give us a lua state to execute the shell in
  ls <- Lua.newstate
  -- this will import screen width and height from lua script
  settings <- importSettings ls "mods/config/"
  -- an initial state
  let state   = genParams $ initState SMenu ls seeds settings
      fs      = settingFullscreen settings
      sw      = settingScreenW settings
      sh      = settingScreenH settings
  -- event channel handles user input, state changes, and loading screens
  eventsChan <- newQueue --newTQueueIO :: IO (TQueue Event)


  -- opens the GLFW and sets the callbacks to handle errors and user input
  GLFW.withWindow fs sw sh "A Bridge Far Away..." $ \window -> do
    GLFW.setErrorCallback              $ Just $ errorCallback       eventsChan
    GLFW.setKeyCallback         window $ Just $ keyCallback         eventsChan
    GLFW.setMouseButtonCallback window $ Just $ mouseButtonCallback eventsChan
    GLFW.setWindowSizeCallback  window $ Just $ reshapeCallback     eventsChan
    GLFW.setScrollCallback      window $ Just $ scrollCallback      eventsChan
    GLFW.swapInterval 0

    -- loads all textures into memory
    (ftex, wtex, utex) <- liftIO $ loadAllTextures window
    -- loads the default font
    let fonts = makeFonts(ftex)

    -- these channels pass data between the main GL thread and the timer threads
    -- these channels are for updating the main game state when changes occur in one of the threads
    stateChan1 <- newChan
    stateChan2 <- newChan
    stateChan3 <- newChan
    stateChan4 <- newChan
    -- these channels handle updating the timer's state (pause, unpause, etc)
    wTimerChan <- newChan
    aTimerChan <- newChan

    --creates the enviornment
    let env = Env { envEventsChan = eventsChan
                  , envWindow     = window
                  , envFonts      = fonts
                  , envWTex       = wtex
                  , envZTex       = [[]]
                  , envUTex       = utex
                  , envZazzTex    = [[[]]]
                  , envStateChan1 = stateChan1
                  , envStateChan2 = stateChan2
                  , envStateChan3 = stateChan3
                  , envStateChan4 = stateChan4
                  , envWTimerChan = wTimerChan
                  , envATimerChan = aTimerChan
                  }

    -- forks the timers initially stopped, 100 is pretty fast, 1000 means one game min = one real sec
    -- the timer states are not guaranteed to be correct, since they do different things, only the main
    -- GL state is correct
    -- timer handles the world events, weather, time, etc. since its slow, its safe to do lots of math here
    forkIO $ (worldTime env state 1000 TStop)
    -- timer handles animation of the units
    forkIO $ (animTime env state 100 TStop)

    -- runs the whole game monad
    void $ evalRWST run env state

-- this is the main GL loop
run :: Game ()
run = do
  env <- ask
  timedLoop $ \lastTick tick -> do
    state <- get
    let framespersecond = (settingFPS (stateSettings state))
    window <- asks envWindow
    liftIO $ do
      GLFW.swapBuffers window
      GLFW.pollGLFWEvents
      getGLErrors
      whileM_ ((\cur -> (cur - tick) < (1.0/framespersecond)) <$> getCurTick) (return ())
      draw (stateGame state) state env
    processEvents
    liftIO $ not <$> GLFW.windowShouldClose window

-- this pattern match on the game mode will chose which screen to draw
draw :: GameState -> State -> Env -> IO ()
draw SMenu      state env = do
  drawMenu state env
draw SShell     state env = do
  drawShell state env
draw SLoadWorld state env = do
  drawLoadScreen state env "Creating World..."
  -- change to world mode using fifo
  atomically $ writeChan (envATimerChan env) TStart
  atomically $ writeChan (envWTimerChan env) TStart
  newstate1 <- atomically $ readChan (envStateChan1 env)
  newstate2 <- atomically $ readChan (envStateChan3 env)
  liftIO $ loadedCallback (envEventsChan env) SLoadTime
draw SLoadTime  state env = do
  drawLoadScreen state env "simulating history"
  -- start the timer once the chan has emptied
  newstate1 <- atomically $ readChan (envStateChan1 env)
  newstate2 <- atomically $ readChan (envStateChan3 env)
  -- set the monad state to timer's state
  let unftime = stateTime state
  liftIO $ timerCallback (envEventsChan env) newstate1
  liftIO $ animCallback  (envEventsChan env) newstate2
  -- let history run a bit
  let settings = stateSettings state
      history  = settingHistory settings
  if unftime > (1+toInteger(history)) then liftIO $ loadedCallback (envEventsChan env) SWorld
  else liftIO $ loadedCallback (envEventsChan env) SLoadTime
draw SWorld     state env = do
  liftIO sceneSetup
  drawWorld   state env
  drawWorldUI state env
  where grid = stateGrid state
draw _ _ _ = do
  print "fuck"

-- this will loop the main loop
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

-- reads the events channel, and executes events in order
processEvents :: Game ()
processEvents = do
  tc <- asks envEventsChan
  me <- liftIO $ atomically $ tryReadQueue tc
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
        liftIO $ GLFW.closeGLFW window
    (EventKey window k _ ks mk) -> do
      when (GLFW.keyPressed ks) $ do
        evalKey window k ks mk
    -- changes the gamestate when we have loaded (sets previous
    -- gamestate so that we can toggle certain screens like the shell)
    (EventLoaded newstate) -> do
      state <- get
      modify $ \s -> s { stateGame     = newstate
                       , stateGamePrev = (stateGame state) }
    -- changes the state from the event queue
    (EventUpdateState state) -> do
      modify $ \s -> s { stateTime = (stateTime state) }
    -- changes the state to reflect animation
    (EventAnimState state) -> do
      modify $ \s -> s { stateCursor = (stateCursor state) }
    -- these should reorganize the screen when the window is resized
    (EventFrameBufferSize _ width height) -> do
      state <- get
      let newsetts = resizeSettings (stateSettings state) width height
      modify $ \s -> s { stateSettings = newsetts }
      liftIO $ adjustWindow state
    (EventWindowResize win w h) -> do
      state <- get
      let newsetts = resizeSettings (stateSettings state) w h
      modify $ \s -> s { stateSettings = newsetts }
      liftIO $ adjustWindow state
