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
import Game.Moon
import Game.Sun
import Game.Ocean
import Game.Map
import Game.Zone

-- the game monad wrapper, gives us a threadsafe state and env
type Game = RWST Env () State IO

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes
    
main :: IO ()
main = do
  -- this will 100% your CPU
  --runOnAllCores

  -- event channel handles user input, state changes, and loading screens
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  -- opens the GLFW and sets the callbacks to handle errors and user input
  withWindow screenw screenh "A Bridge Far Away..." $ \window -> do
    initWindow
    GLFW.setErrorCallback             $ Just $ errorCallback   eventsChan
    GLFW.setKeyCallback        window $ Just $ keyCallback     eventsChan
    GLFW.setWindowSizeCallback window $ Just $ reshapeCallback eventsChan
    GLFW.swapInterval 0

    -- only loads ttf fonts
    f1 <- loadFont "data/fonts/cheque/Cheque-Regular.ttf"
    f2 <- loadFont "data/fonts/smone/SupermercadoOne-Regular.ttf"

    -- load textures for the world and zones respectively
    (wtex, ztex) <- liftIO $ initTexs window

    -- makes some std gens for most of the RNG
    s1 <- newStdGen
    s2 <- newStdGen
    s3 <- newStdGen
    s4 <- newStdGen
    s5 <- newStdGen
    s6 <- newStdGen
    
    -- these are some of the parameters that are regenerated with new maps
    let nconts = (randomRs (minnconts, maxnconts) (mkStdGen 42)) !! 1
    let rangers = (randomRs (minnconts, maxnconts) (mkStdGen 43))
    let sol = (makeSun 0.0 ((fromIntegral(gridh))/2) 800 600)
        luna = (makeMoon 0.0 ((fromIntegral(gridh)/30)+(fromIntegral(gridh))/2) 20 10)
    -- this generates most of the parameters, the above 
        state = genParams SMenu 1 nconts 0 rangers sol luna s1 s2 s3 s4 s5 s6

    -- these channels pass data between the main thread and the timer thread
    -- this channel is for updating the main game state when changes occur
    stateChan1 <- atomically $ newTChan
    -- this channel is for updating the timer's state when it is started/stopped
    stateChan2 <- atomically $ newTChan
    -- this channel broadcasts the time as an integer,
    -- seperated out for possible performance boost, im not sure if it makes a difference
    timerChan  <- atomically $ newTChan

    -- the enviornment for the game is read-only, nothing here will ever change
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

    -- the timer is forked initially stopped.  100 is pretty fast, 1000 is 1 game min = 1 real sec
    -- the timer state is not guaranteed to be correct, some things dont make sense
    -- for it to deal with and so it doesnt.
    forkIO $ (gameTime env state 1200 TStop)
    -- runs the whole monad
    void $ evalRWST run env state

-- this is the main GL loop
run :: Game ()
run = do
  env <- ask
  -- the timed loop will try and maintain 60fps
  timedLoop $ \lastTick tick -> do
    state <- get
    window <- asks envWindow
    liftIO $ do
      GLFW.swapBuffers window
      GLFW.pollEvents
      err <- GL.get GL.errors
      mapM_ print err
      whileM_ ((\cur -> (cur - tick) < (1.0/60.0)) <$> getCurTick) (return ())
    -- this is where we process user input and update the state
      draw (stateGame state) state env
    processEvents
    liftIO $ not <$> GLFW.windowShouldClose window

-- pattern match on the state of the game draws different screens for different modes
draw :: GameState -> State -> Env -> IO ()
draw SMenu state env = do
  beginDrawText
  drawText (envFontBig env) 1 95 72 72 "A Bridge Far Away..."
  drawText (envFontSmall env) 1 75 36 36 "press c to create a world"
  drawText (envFontSmall env) 1 60 36 36 "press l to load a world"
  drawText (envFontSmall env) 1 45 36 36 "press esc to quit"
draw SLoad state env = do
  beginDrawText
  drawText (envFontBig env) 1 95 72 72 "Loading..."
  drawText (envFontSmall env) 1 75 36 36 "Creating World..."
  -- change modes to the world mode, again in the fifo
  atomically $ writeTChan (envTimerChan env) TStart
  newstate <- atomically $ readTChan (envStateChan1 env)
  liftIO $ loadedCallback (envEventsChan env) SLoadTime
-- this doesnt quite work right yet, this screen is only shown for an instance
draw SLoadTime state env = do
  beginDrawText
  drawText (envFontBig env) 1 95 72 72 "Loading..."
  drawText (envFontSmall env) 1 75 36 36 "Simulating History..."
  -- start the timer once the chan has emptied (since its fifo)
  newstate <- atomically $ readTChan (envStateChan1 env)
  -- set the monad state to the timer's state, (this fixes the notorious loading screen bug)
  let unftime = stateTime state
  liftIO $ timerCallback (envEventsChan env) newstate
  -- wait until after the history has been running for a while
  if unftime > (1+toInteger(history)) then liftIO $ loadedCallback (envEventsChan env) SWorld
  else liftIO $ loadedCallback (envEventsChan env) SLoadTime
draw SLoadZone state env = do
  beginDrawText
  drawText (envFontBig env) 1 95 72 72 "Loading..."
  drawText (envFontSmall env) 1 75 36 36 "Generating Zone..."
  liftIO $ loadedCallback (envEventsChan env) SZone
draw SLoadElev state env = do
  beginDrawText
  drawText (envFontBig env) 1 95 72 72 "Loading Elevation..."
  -- since the timer is running on its own, and updating the fifo queue,
  -- we dont have to update the state here, it will update in 1/60 ticks
  -- once we enter the SElev state
  liftIO $ loadedCallback (envEventsChan env) SElev
draw SLoadSeaTemp state env = do
  beginDrawText
  drawText (envFontBig env) 1 95 72 72 "Loading..."
  drawText (envFontSmall env) 1 75 36 36 "Calculating Ocean Temperatures..."
  liftIO $ loadedCallback (envEventsChan env) SSeaTemp
draw SLoadSeaCurrents state env = do
  beginDrawText
  drawText (envFontBig env) 1 95 72 72 "Loading..."
  drawText (envFontSmall env) 1 75 36 36 "Calculating Ocean Currents..."
  liftIO $ loadedCallback (envEventsChan env) SSeaCurrents
draw SWorld state env = do
  GL.clear[GL.ColorBuffer, GL.DepthBuffer]
  -- read in a non blocking way, maintaining the old state if the channel is empty
  statebuff <- atomically $ tryReadTChan (envStateChan1 env)
  newstate <- case (statebuff) of
    Nothing -> return state
    Just n  -> return n
  let unftime = stateTime newstate
  beginDrawText
  drawText (envFontSmall env) (-120) (-40) 36 36 $ formatTime unftime
  drawText (envFontSmall env) (-120) (-25) 36 36 $ "x:" ++ (show (fst (stateCursor state))) ++ " y:" ++ (show (snd (stateCursor state)))
  GL.preservingMatrix $ do
    drawScene newstate (envWTex env)
  GL.preservingMatrix $ do
  -- the cursor must use the old state, since the timer has no concept of where the
  -- cursor is moving.  it could be updated, but as of now there is no need
    drawCursor state (envWTex env)
  -- this will change the state to either the new state from the timer, or the old state
  liftIO $ timerCallback (envEventsChan env) newstate
draw SZone state env = do
  GL.clear[GL.ColorBuffer, GL.DepthBuffer]
  statebuff <- atomically $ tryReadTChan (envStateChan1 env)
  newstate <- case (statebuff) of
    Nothing -> return state
    Just n  -> return n
  let unftime = stateTime newstate
      sun     = stateSun newstate
  beginDrawText
  drawText (envFontSmall env) (-120) (-40) 36 36 $ formatTime unftime
  GL.preservingMatrix $ do
    drawZone state (envZTex env)
  GL.preservingMatrix $ do
    drawZoneCursor state (envZTex env)
  liftIO $ timerCallback (envEventsChan env) newstate
draw SElev state env = do
  GL.clear[GL.ColorBuffer, GL.DepthBuffer]
  statebuff <- atomically $ tryReadTChan (envStateChan1 env)
  newstate <- case (statebuff) of
    Nothing -> return state
    Just n  -> return n
  let unftime = stateTime newstate
      sun     = stateSun newstate
  beginDrawText
  drawText (envFontSmall env) (-120) (-40) 36 36 $ formatTime unftime
  drawText (envFontSmall env) (-120) (-25) 36 36 $ "x:" ++ (show (fst (stateCursor state))) ++ " y:" ++ (show (snd (stateCursor state)))
  drawText (envFontSmall env) (-120) (-10) 36 36 $ formatElev (stateElev newstate) (stateCursor state)
  GL.preservingMatrix $ do
    drawElev newstate (envWTex env)
  GL.preservingMatrix $ do
    drawCursor state (envWTex env)
  liftIO $ timerCallback (envEventsChan env) newstate
draw SSeaTemp state env = do
  GL.clear[GL.ColorBuffer, GL.DepthBuffer]
  statebuff <- atomically $ tryReadTChan (envStateChan1 env)
  newstate <- case (statebuff) of
    Nothing -> return state
    Just n  -> return n
  let unftime = stateTime newstate
      sun     = stateSun newstate
  beginDrawText
  drawText (envFontSmall env) (-120) (-40) 36 36 $ formatTime unftime
  drawText (envFontSmall env) (-120) (-25) 36 36 $ "x:" ++ (show (fst (stateCursor state))) ++ " y:" ++ (show (snd (stateCursor state)))
  -- the ocean temp z will give the temperature of the 5 different zones of the sea
  drawText (envFontSmall env) (-120) (-55) 36 36 $ formatOceanTemp (stateOceanTempZ state) (stateOceans state) (stateCursor state)
  GL.preservingMatrix $ do
    drawOcean state (envWTex env)
  GL.preservingMatrix $ do
    drawCursor state (envWTex env)
  liftIO $ timerCallback (envEventsChan env) newstate
draw SSeaCurrents state env = do
  GL.clear[GL.ColorBuffer, GL.DepthBuffer]
  statebuff <- atomically $ tryReadTChan (envStateChan1 env)
  newstate <- case (statebuff) of
    Nothing -> return state
    Just n  -> return n
  let unftime = stateTime newstate
      sun     = stateSun newstate
  beginDrawText
  drawText (envFontSmall env) (-120) (-40) 36 36 $ formatTime unftime
  drawText (envFontSmall env) (-120) (-25) 36 36 $ "x:" ++ (show (fst (stateCursor state))) ++ " y:" ++ (show (snd (stateCursor state)))
  -- the ocean temp z will give the temperature of the 5 different zones of the sea
  drawText (envFontSmall env) (-120) (-55) 36 36 $ formatOceanCurrents (stateOceanCurrentsZ state) (stateOceans state) (stateCursor state)
  GL.preservingMatrix $ do
    drawOceanCurrents state (envWTex env)
  GL.preservingMatrix $ do
    drawCursor state (envWTex env)
  liftIO $ timerCallback (envEventsChan env) newstate
draw _ _ _ = do
  -- i have yet to see this called, that is good...
  print "fuck"

-- this will run on all cores, not recommended
runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities

-- this will loop the main GL loop
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

-- reads the event channel, and executes each event in order
processEvents :: Game ()
processEvents = do
  tc <- asks envEventsChan
  me <- liftIO $ atomically $ tryReadTQueue tc
  case me of
    Just e -> do
      processEvent e
      processEvents
    Nothing -> return ()

-- one big case statement on the event that occured in the events channel
processEvent :: Event -> Game ()
processEvent ev =
  case ev of
    (EventError e s) -> do
      window <- asks envWindow
      liftIO $ do
        putStrLn $ "error " ++ show e ++ " " ++ show s
        GLFW.setWindowShouldClose window True
    (EventKey window k _ ks mk) -> do
      when (ks == GLFW.KeyState'Pressed) $ do
        state <- get
        env   <- ask
        -- exits game
        when (((stateGame state) == SMenu) && (k == GLFW.Key'Escape)) $ do
            liftIO $ GLFW.setWindowShouldClose window True
        -- creates a new world
        when (((stateGame state) == SMenu) && (k == GLFW.Key'C)) $ do
            liftIO $ loadedCallback (envEventsChan env) SLoad
            let newstate = initWorld state env
            -- let the timer know that we have generated a new state
            liftIO $ atomically $ writeTChan (envStateChan2 env) newstate
            modify $ \s -> s { stateGrid = (stateGrid newstate)
                             , stateElev = (stateElev newstate)
                             }
        -- regenerates the world
        when (((stateGame state) == SWorld) && (k == GLFW.Key'R)) $ do
            -- stops the timer
            liftIO $ atomically $ writeTChan (envTimerChan env) TStop
            -- ensure that nothing has been accumulating in the channels
            liftIO $ emptyChan (envStateChan1 env)
            liftIO $ emptyChan (envStateChan2 env)
            liftIO $ loadedCallback (envEventsChan env) SLoad
            let newstate = regenWorld state env
            -- just to be sure, we clear the channels again
            liftIO $ emptyChan (envStateChan1 env)
            liftIO $ emptyChan (envStateChan2 env)
            liftIO $ atomically $ writeTChan (envStateChan2 env) newstate
            modify $ \s -> newstate
        -- enter a zone
        when (((stateGame state) == SWorld) && (k == GLFW.Key'Enter)) $ do
            let z     = generateZone state
                oldzs = stateZones state
            liftIO $ loadedCallback (envEventsChan env) SLoadZone
            modify $ \s -> s { stateZones = (z:oldzs) }
        -- displays the elevation in meters
        when (((stateGame state) == SWorld) && (k == GLFW.Key'E)) $ do
            modify $ \s -> s { stateGame = SLoadElev }
        -- displays the ocean temp in C at 5 different depths
        when (((stateGame state) == SWorld) && (k == GLFW.Key'O)) $ do
            modify $ \s -> s { stateGame = SLoadSeaTemp }
        -- displays the ocean currents with arrows
        when (((stateGame state) == SWorld) && (k == GLFW.Key'I)) $ do
            modify $ \s -> s { stateGame = SLoadSeaCurrents }
        -- moves the cursor with left, right, up, down, or h,l,k,j
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Left) || (k == GLFW.Key'H))) $ do
            modify $ \s -> s { stateCursor = (moveCursor 1 (stateCursor state) West) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Right) || (k == GLFW.Key'L))) $ do
            modify $ \s -> s { stateCursor = (moveCursor 1 (stateCursor state) East) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Up) || (k == GLFW.Key'K))) $ do
            modify $ \s -> s { stateCursor = (moveCursor 1 (stateCursor state) North) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Down) || (k == GLFW.Key'J))) $ do
            modify $ \s -> s { stateCursor = (moveCursor 1 (stateCursor state) South) }
        -- moves the cursor 10 with the shift key
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Left) || (k == GLFW.Key'H)) && (GLFW.modifierKeysShift mk)) $ do
            modify $ \s -> s { stateCursor = (moveCursor 9 (stateCursor state) West) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Right) || (k == GLFW.Key'L)) && (GLFW.modifierKeysShift mk)) $ do
            modify $ \s -> s { stateCursor = (moveCursor 9 (stateCursor state) East) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Up) || (k == GLFW.Key'K)) && (GLFW.modifierKeysShift mk)) $ do
            modify $ \s -> s { stateCursor = (moveCursor 9 (stateCursor state) North) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Down) || (k == GLFW.Key'J)) && (GLFW.modifierKeysShift mk)) $ do
            modify $ \s -> s { stateCursor = (moveCursor 9 (stateCursor state) South) }
        -- moves the camera when in a zone
        when ((((stateGame state) == SZone) && ((k == GLFW.Key'Left) || (k == GLFW.Key'H))) && (GLFW.modifierKeysControl mk)) $ do
            modify $ \s -> s { stateZones = ((moveZoneCam (16.0/32.0) (head (stateZones state)) West):(tail (stateZones state))) }
        when ((((stateGame state) == SZone) && ((k == GLFW.Key'Right) || (k == GLFW.Key'L))) && (GLFW.modifierKeysControl mk)) $ do
            modify $ \s -> s { stateZones = ((moveZoneCam (16.0/32.0) (head (stateZones state)) East):(tail (stateZones state))) }
        when ((((stateGame state) == SZone) && ((k == GLFW.Key'Up) || (k == GLFW.Key'K))) && (GLFW.modifierKeysControl mk)) $ do
            modify $ \s -> s { stateZones = ((moveZoneCam (16.0/32.0) (head (stateZones state)) North):(tail (stateZones state))) }
        when ((((stateGame state) == SZone) && ((k == GLFW.Key'Down) || (k == GLFW.Key'J))) && (GLFW.modifierKeysControl mk)) $ do
            modify $ \s -> s { stateZones = ((moveZoneCam (16.0/32.0) (head (stateZones state)) South):(tail (stateZones state))) }

        -- exits the elevation screen
        when (((stateGame state) == SElev) && ((k == GLFW.Key'E) || (k == GLFW.Key'Escape))) $ do
            modify $ \s -> s { stateGame = SWorld }
        -- exits the sea temperature screen
        when (((stateGame state) == SSeaTemp) && ((k == GLFW.Key'O) || (k == GLFW.Key'Escape))) $ do
            modify $ \s -> s { stateGame = SWorld }
        -- exits the sea currents screen
        when (((stateGame state) == SSeaCurrents) && ((k == GLFW.Key'I) || (k == GLFW.Key'Escape))) $ do
            modify $ \s -> s { stateGame = SWorld }
        -- moves the Z level of the Sea currents viewer up
        when (((stateGame state) == SSeaCurrents) && ((k == GLFW.Key'U))) $ do
            modify $ \s -> s { stateOceanCurrentsZ = (decreaseOceanZ (stateOceanCurrentsZ state)) }
        -- moves the Z level of the Sea currents viewer down
        when (((stateGame state) == SSeaCurrents) && ((k == GLFW.Key'M))) $ do
            modify $ \s -> s { stateOceanCurrentsZ = (increaseOceanZ (stateOceanCurrentsZ state)) }
        -- moves the Z level of the Sea temp viewer up
        when (((stateGame state) == SSeaTemp) && ((k == GLFW.Key'U))) $ do
            modify $ \s -> s { stateOceanTempZ = (decreaseOceanZ (stateOceanTempZ state)) }
        -- moves the Z level of the Sea temp viewer down
        when (((stateGame state) == SSeaTemp) && ((k == GLFW.Key'M))) $ do
            modify $ \s -> s { stateOceanTempZ = (increaseOceanZ (stateOceanTempZ state)) }
        -- exits the game, in future, this should save
        when (((stateGame state) == SWorld) && (k == GLFW.Key'Escape)) $ do
            liftIO $ GLFW.setWindowShouldClose window True
        -- exits zone view into world view
        when (((stateGame state) == SZone) && (k == GLFW.Key'Escape)) $ do
            modify $ \s -> s { stateGame = SWorld }
      when (ks == GLFW.KeyState'Repeating) $ do
        state <- get
        env   <- ask
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Left) || (k == GLFW.Key'H))) $ do
            modify $ \s -> s { stateCursor = (moveCursor 1 (stateCursor state) West) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Right) || (k == GLFW.Key'L))) $ do
            modify $ \s -> s { stateCursor = (moveCursor 1 (stateCursor state) East) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Up) || (k == GLFW.Key'K))) $ do
            modify $ \s -> s { stateCursor = (moveCursor 1 (stateCursor state) North) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Down) || (k == GLFW.Key'J))) $ do
            modify $ \s -> s { stateCursor = (moveCursor 1 (stateCursor state) South) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Left) || (k == GLFW.Key'H)) && (GLFW.modifierKeysShift mk)) $ do
            modify $ \s -> s { stateCursor = (moveCursor 9 (stateCursor state) West) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Right) || (k == GLFW.Key'L)) && (GLFW.modifierKeysShift mk)) $ do
            modify $ \s -> s { stateCursor = (moveCursor 9 (stateCursor state) East) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Up) || (k == GLFW.Key'K)) && (GLFW.modifierKeysShift mk)) $ do
            modify $ \s -> s { stateCursor = (moveCursor 9 (stateCursor state) North) }
        when ((((stateGame state) == SWorld) || ((stateGame state) == SElev) || ((stateGame state) == SSeaTemp) || ((stateGame state) == SSeaCurrents)) && ((k == GLFW.Key'Down) || (k == GLFW.Key'J)) && (GLFW.modifierKeysShift mk)) $ do
            modify $ \s -> s { stateCursor = (moveCursor 9 (stateCursor state) South) }
        when ((((stateGame state) == SZone) && ((k == GLFW.Key'Left) || (k == GLFW.Key'H))) && (GLFW.modifierKeysControl mk)) $ do
            modify $ \s -> s { stateZones = ((moveZoneCam (2.0/32.0) (head (stateZones state)) West):(tail (stateZones state))) }
        when ((((stateGame state) == SZone) && ((k == GLFW.Key'Right) || (k == GLFW.Key'L))) && (GLFW.modifierKeysControl mk)) $ do
            modify $ \s -> s { stateZones = ((moveZoneCam (2.0/32.0) (head (stateZones state)) East):(tail (stateZones state))) }
        when ((((stateGame state) == SZone) && ((k == GLFW.Key'Up) || (k == GLFW.Key'K))) && (GLFW.modifierKeysControl mk)) $ do
            modify $ \s -> s { stateZones = ((moveZoneCam (2.0/32.0) (head (stateZones state)) North):(tail (stateZones state))) }
        when ((((stateGame state) == SZone) && ((k == GLFW.Key'Down) || (k == GLFW.Key'J))) && (GLFW.modifierKeysControl mk)) $ do
            modify $ \s -> s { stateZones = ((moveZoneCam (2.0/32.0) (head (stateZones state)) South):(tail (stateZones state))) }

    -- these should reorganize the screen when you resize the window
    (EventFramebufferSize _ width height) -> do
      adjustWindow
    (EventWindowResize win w h) -> do
      adjustWindow
    -- changes the state of the game from the event queue
    (EventLoaded state) -> do
      modify $ \s -> s { stateGame = state }
    -- changes the state of the timer related stuff through the event queue
    (EventUpdateState state) -> do
      modify $ \s -> s { stateGrid     = (stateGrid state)
                       , stateElev     = (stateElev state)
                       , stateSun      = (stateSun state)
                       , stateSunSpots = (stateSunSpots state)
                       , stateTime     = (stateTime state)
                       , stateOceans   = (stateOceans state)
                       , stateSkies    = (stateSkies state)
                       }

-- empties a channel bu reading everything left recursively
emptyChan :: TChan a -> IO ()
emptyChan chan = do
  test <- (atomically (isEmptyTChan chan))
  if test then return ()
  else do
    s <- atomically $ readTChan chan
    emptyChan chan

-- this should reorganize everything when the window is resized, in practice
-- it only works somewhat
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

-- initializes the window before we have the game monad
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

-- uses GLFW to create the window, and destroy it when closed
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

-- error callback to register errors
errorCallback :: TQueue Event -> GLFW.Error -> String -> IO ()
errorCallback tc e s = atomically $ writeTQueue tc $ EventError e s
-- registers key states on input
keyCallback :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey win k sc ka mk
-- called when the window is resized
reshapeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
reshapeCallback tc win w h = atomically $ writeTQueue tc $ EventWindowResize win w h
-- call to change the state
loadedCallback :: TQueue Event -> GameState -> IO ()
loadedCallback tc state = atomically $ writeTQueue tc $ EventLoaded state
-- changes parts of the state that the timer changes
timerCallback :: TQueue Event -> State -> IO ()
timerCallback tc state = atomically $ writeTQueue tc $ EventUpdateState state
