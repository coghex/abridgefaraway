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

import qualified GLUtil.ABFA as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import GLUtil.ABFA
import GLUtil.Font
import GLUtil.Util
import ABFA.Game
import ABFA.Event
import ABFA.Settings
import ABFA.State
import ABFA.UI

main :: IO ()
main = do
  -- this will import screen width and height from lua script
  settings <- importSettings
  let state   = initState SMenu settings
      fs      = fullscreen settings
      sw      = screenw settings
      sh      = screenh settings
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
    ftex <- liftIO $ loadAllTextures window
    -- loads the default font
    let fonts = makeFonts(ftex)

    --creates the enviornment
    let env = Env { envEventsChan = eventsChan
                  , envWindow     = window
                  , envFonts      = fonts
                  }
    -- runs the whole monad
    void $ evalRWST run env state

-- this is the main GL loop
run :: Game ()
run = do
  env <- ask
  timedLoop $ \lastTick tick -> do
    state <- get
    let framespersecond = (fps (stateSettings state))
    window <- asks envWindow
    liftIO $ do
      GLFW.swapBuffers window
      GLFW.pollGLFWEvents
      getGLErrors
      whileM_ ((\cur -> (cur - tick) < (1.0/framespersecond)) <$> getCurTick) (return ())
      draw (stateGame state) state env
    processEvents
    liftIO $ not <$> GLFW.windowShouldClose window

draw :: GameState -> State -> Env -> IO ()
draw SMenu state env = do
  drawMenu state env
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
        liftIO $ GLFW.closeGLFW window--GLFW.setWindowShouldClose window True
    (EventKey window k _ ks mk) -> do
      when (GLFW.keyPressed ks) $ do
      --when (ks == GLFW.KeyState'Pressed) $ do
        state <- get
        env   <- ask
        -- exits game
        when (((stateGame state) == SMenu) && (GLFW.keyEscape k)) $ do
          liftIO $ GLFW.closeGLFW window--liftIO $ GLFW.setWindowShouldClose window True
