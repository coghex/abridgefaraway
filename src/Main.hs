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
import Game.Zone
import Game.Road
import Game.Trees

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
    (wtexs, ztexs) <- liftIO $ initTexs window
    s1 <- newStdGen
    s2 <- newStdGen
    s3 <- newStdGen
    s4 <- newStdGen
    s5 <- newStdGen
    s6 <- newStdGen
    nconts <- randomN minnconts maxnconts
    inspots <- randomN mininspots maxinspots
    zspots <- randomN minzspots maxzspots
    nbush <- randomN minnbushes maxnbushes
    nroads <- randomN minroads maxroads
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
        bushes = buildList2 ((randomList (0, zonew) nbush s3), (randomList (0, zoneh) nbush s4))
        brands = buildList2 ((randomList (1, (zonew-1)) nbush s6), (randomList (1, (zoneh-1)) nbush s5))
        bsiz = randomList (minbushsize, maxbushsize) nbush s6
        pathrands = take nroads (repeat (randomList (0, npaths) pathlen s1))
        roads = buildList2 ((randomList (2, (zonew-2)) nroads s1), (randomList (2, (zoneh-2)) nroads s2))
        trees = (take (zonew*zoneh) (repeat 0))
        
        
    let env = Env
            { envEventsChan   = eventsChan
            , envWindow       = window
            , envFontBig      = f1
            , envFontSmall    = f2
            , envWTexs        = wtexs
            , envZTexs        = ztexs
            , envSeeds        = [s1, s2, s3, s4, s5, s6]
            }
        state = State
            { stateGame       = SMenu
            , stateScreenW    = screenw
            , stateScreenH    = screenh
            , stateGrid       = (take (gridw*gridh) (repeat 4))
            , stateCursor     = (5, 5)
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
            , stateZone       = (take (gridw*gridh) (repeat 0))
            , stateZoneCamx   = 0.0
            , stateZoneCamy   = 0.0
            , stateZoneCamz   = 0.0
            , stateCurrentZ   = (take (zonew*zoneh) (repeat 0))
            , stateBushes     = bushes
            , stateBRands     = brands
            , stateBSizes     = bsiz
            , statePaths      = (take (zonew*zoneh) (repeat 0))
            , statePathRands  = pathrands
            , stateRoads      = roads
            , stateTrees      = trees
            , stateTreeRands  = []
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
    drawText (envFontSmall env) 1 55 12 36 "press space to return to menu"
draw SLoad = do
  env   <- ask
  state <- get
  liftIO $ do
    beginDrawText
    drawText (envFontBig env) 1 95 24 72 "Loading..."
    liftIO $ loadedCallback (envEventsChan env) SWorld
draw SWorld = do
  env   <- ask
  state <- get
  liftIO $ do
    GL.clear[GL.ColorBuffer, GL.DepthBuffer]
    GL.preservingMatrix $ do
      drawScene state (envWTexs env)
    GL.preservingMatrix $ do
      drawCursor state (envWTexs env)
draw SZone  = do
  env   <- ask
  state <- get
  liftIO $ do
    GL.clear[GL.ColorBuffer, GL.DepthBuffer]
    GL.preservingMatrix $ do
      drawZone state (stateZoneCamx state) (stateZoneCamy state) (envZTexs env)
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
        when (k == GLFW.Key'Escape && (stateGame state) == SWorld) $ do
          liftIO $ GLFW.setWindowShouldClose window True
          liftIO $ saveMap (stateGrid state)
          liftIO $ saveZone (fst (stateCursor state)) (snd (stateCursor state)) (stateCurrentZ state) (statePaths state) (stateBushes state) (stateBRands state) (stateBSizes state)
        when (k == GLFW.Key'Escape) $ do
          liftIO $ GLFW.setWindowShouldClose window True
        when ((k == GLFW.Key'Space)&&((stateGame state)==SZone)) $ do
          modify $ \s -> s { stateGame = SLoad }
          liftIO $ saveZone (fst (stateCursor state)) (snd (stateCursor state)) (stateCurrentZ state) (statePaths state) (stateBushes state) (stateBRands state) (stateBSizes state)
        when ((k == GLFW.Key'Enter) && ((stateGame state) == SWorld)) $ do
          x <- liftIO $ loadZone (fst (stateCursor state)) (snd (stateCursor state))
          if (x==[]) then do
            let newzones = setZone (stateZone state) (fst (stateCursor state)) (snd (stateCursor state))
            sg <- liftIO $ newStdGen
            nbush <- liftIO $ randomN minnbushes maxnbushes
            spr <- liftIO $ newRandomList ((take (zonew*zoneh) (repeat 0)))
            let bushes = buildList2 ((randomList (0, zonew) nbush sg), (randomList (0, zoneh) nbush sg))
            let brands = buildList2 ((randomList (1, (zonew-1)) nbush sg), (randomList (1, (zoneh-1)) nbush sg))
            let bsiz = randomList (minbushsize, maxbushsize) nbush sg
            let rand = randomList (10, 15) (zonew*zoneh) sg
            --let newpaths = roadMapper (stateRoads state) ((take (zonew*zoneh) (repeat 0))) (spr)
            --let newpaths = initPath (zonewover2) (zonehover2) 0 0 ((take (zonew*zoneh) (repeat 0))) (spr!!1)
            let newcurrentzone = initZone state rand 0
            let newpaths = initDoorPaths newcurrentzone spr
            let p0 = fixPath newpaths (initZone state rand 0)
            let strs = buildList2 ((randomList (0, zonew) ntree sg), (randomList (1, zoneh) ntree sg))
            let trees = initTrees strs newcurrentzone
            liftIO $ print newcurrentzone
            modify $ \s -> s { stateZone = newzones
                             , stateCurrentZ = newcurrentzone
                             , stateGame = SZone
                             , statePaths = p0
                             , statePathRands = spr
                             , stateBushes = bushes
                             , stateBRands = brands
                             , stateBSizes = bsiz
                             , stateTrees  = trees
                             , stateTreeRands = strs
                             }
          else do
            x2 <- liftIO $ loadPath (fst (stateCursor state)) (snd (stateCursor state))
            modify $ \s -> s { stateCurrentZ = x
                             , stateGame = SZone
                             , statePaths = x2
                             }
        when ((k == GLFW.Key'Space)&&((stateGame state)==SWorld)) $ do
          modify $ \s -> s { stateGame = SMenu }
        when (k == GLFW.Key'C && (stateGame state) == SMenu) $ do
          modify $ \s -> s { stateGame = SLoad }
          let newstate = initWorld state (length (stateConts state))
          modify $ \s -> s { stateGrid = (stateGrid newstate) }
        when ((k == GLFW.Key'L) && ((stateGame state) == SMenu)) $ do
          x <- liftIO loadMap
          modify $ \s -> s { stateGrid = x
                           , stateGame = SWorld }
        when ((k == GLFW.Key'Right) && ((stateGame state) == SWorld) && (fst (stateCursor state) <= (gridw-2))) $
          modify $ \s -> s { stateCursor = ((fst (stateCursor state))+1, ((snd (stateCursor state)))) }
        when ((k == GLFW.Key'Left) && ((stateGame state) == SWorld) && (fst (stateCursor state) > (0))) $
          modify $ \s -> s { stateCursor = ((fst (stateCursor state))-1, ((snd (stateCursor state)))) }

        when ((k == GLFW.Key'Up) && ((stateGame state) == SWorld) && (snd (stateCursor state) <= (gridh-2))) $
          modify $ \s -> s { stateCursor = (fst (stateCursor state), ((snd (stateCursor state))+1)) }
        when ((k == GLFW.Key'Down) && ((stateGame state) == SWorld) && (snd (stateCursor state) > (0))) $
          modify $ \s -> s { stateCursor = (fst (stateCursor state), ((snd (stateCursor state))-1)) }
        when ((k==GLFW.Key'Right) && ((stateGame state) == SZone)) $ do
          modify $ \s -> s { stateZoneCamx = ((stateZoneCamx state) - 1.0) }
        when ((k==GLFW.Key'Left) && ((stateGame state) == SZone)) $ do
          modify $ \s -> s { stateZoneCamx = ((stateZoneCamx state) + 1.0) }
        when ((k==GLFW.Key'Up) && ((stateGame state) == SZone)) $ do
          modify $ \s -> s { stateZoneCamy = ((stateZoneCamy state) - 1.0) }
        when ((k==GLFW.Key'Down) && ((stateGame state) == SZone)) $ do
          modify $ \s -> s { stateZoneCamy = ((stateZoneCamy state) + 1.0) }

    (EventFramebufferSize _ width height) -> do
      adjustWindow
    (EventWindowResize win w h) -> do
      adjustWindow
    (EventLoaded state) -> do
      modify $ \s -> s { stateGame = state }

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
loadedCallback :: TQueue Event -> GameState -> IO ()
loadedCallback tc state = atomically $ writeTQueue tc $ EventLoaded state


