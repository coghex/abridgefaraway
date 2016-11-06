module Main (main) where

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

import World
import Util
import State
import Draw
import Rand
import Settings
import Namegen
import Names
import Font
import Save

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
    GLFW.setErrorCallback             $ Just $ errorCallback eventsChan
    GLFW.setKeyCallback        window $ Just $ keyCallback   eventsChan
    GLFW.setWindowSizeCallback window $ Just $ reshapeCallback eventsChan
    GLFW.swapInterval 0
    (texs, alph) <- liftIO $ initTexs window
    seed <- newStdGen
    s1 <- newStdGen
    s2 <- newStdGen

    continents <- randomN 10 25
    nzazzs <- randomN 10 maxnzazzs
    nspotscont <- randomN 20 maxnspots

    ns1 <- randomN 1 100
    ns2 <- randomN 1 99
    ns3 <- randomN 1 98
    seed1 <- randomN 1 8
    seed2 <- (randomN 2 9)
    seed3 <- (randomN 3 10)
    seed4 <- (randomN 4 11)
    seed5 <- (randomN 5 12)
    seed6 <- (randomN 6 13)
    seed7 <- (randomN 7 14)
    seed8 <- (randomN 8 15)
    seed9 <- (randomN 9 16)
    seed10 <- (randomN 10 17)
    seed11 <- (randomN 11 18)
    seed12 <- (randomN 12 19)
    seed13 <- (randomN 13 20)
    seed14 <- (randomN 14 21)
    let genperson1 = makePerson 10 Male (ns1+1) (ns2+1) (ns3+1) (seed1)
    printPerson genperson1
    let genperson2 = makePerson 12 Female (ns2+2) (ns3+2) (ns1+2) (seed2-1)
    printPerson genperson2
    let genperson3 = makePerson 10 Male (ns1+3) (ns2+3) (ns3+3) (seed3-2)
    printPerson genperson3
    let genperson4 = makePerson 12 Female (ns2+4) (ns3+4) (ns1+4) (seed4-3)
    printPerson genperson4
    let genperson5 = makePerson 10 Male (ns1+5) (ns2+5) (ns3+5) (seed5-4)
    printPerson genperson5
    let genperson6 = makePerson 12 Female (ns2+6) (ns3+6) (ns1+6) (seed6-5)
    printPerson genperson6
    let genperson7 = makePerson 10 Male (ns1+7) (ns2+7) (ns3+7) (seed7-6)
    printPerson genperson7
    let genperson8 = makePerson 12 Female (ns2+8) (ns3+8) (ns1+8) (seed8-7)
    printPerson genperson8
    let genperson9 = makePerson 10 Male (ns1+9) (ns2+9) (ns3+9) (seed9-8)
    printPerson genperson9
    let genperson10 = makePerson 12 Female (ns2+10) (ns3+10) (ns1+10) (seed10-9)
    printPerson genperson10
    let genperson11 = makePerson 10 Male (ns1+11) (ns2+11) (ns3+11) (seed11-10)
    printPerson genperson11
    let genperson12 = makePerson 12 Female (ns2+12) (ns3+12) (ns1+12) (seed12-11)
    printPerson genperson12
    let genperson13 = makePerson 10 Male (ns1+13) (ns2+13) (ns3+13) (seed13-12)
    printPerson genperson13
    let genperson14 = makePerson 12 Female (ns2+14) (ns3+14) (ns1+14) (seed14-13)
    printPerson genperson14


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
            { envEventsChan   = eventsChan
            , envWindow       = window
            , envGridWidth    = gridw
            , envGridHeight   = gridh
           }
        state = State
            { stateGrid       = l
            , stateScreenWidth  = screenw
            , stateScreenHeight = screenh
            , stateTexs       = texs
            , stateGame       = SMenu
            , stateConts      = contl1
            , stateSeeds      = makeSeeds contl2 0 s1 s2 nspots
            , stateRands      = makeSeeds contl3 (2*continents) s1 s2 nspots
            , stateTileSizes  = (randomList (0, maxcontsize) (gridw*gridh+fudge) seed)
            , stateTileRands  = (randomList (0, ntiles) (continents+1) seed)
            , stateContSizes  = contl4
            , stateSIceSizes  = contl5
            , stateSIces      = contl6
            , stateSIceRands  = contl7
            , stateNIceSizes  = contl8
            , stateNIces      = contl9
            , stateNIceRands  = contl10
            , stateZazzs      = contl11
            , stateZazzSizes  = contl12
            , stateZazzRands  = contl13
            , stateZazzTypes  = contl14
            , stateCursor     = False
            , stateCursorX    = 1
            , stateCursorY    = 1
            , stateAlphabet   = alph
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
  if ((((round(100*tick)) `mod` 10)<2) && (((round(100*tick)) `mod` 10)>=0)) then do
    if ((stateCursor state)==True) then do
      modify $ \s -> s { stateCursor = False }
    else
      modify $ \s -> s { stateCursor = True }
  else
    modify $ \s -> s { stateCursor = (stateCursor state) }
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
    sceneSetup
    GL.preservingMatrix $ do
      drawScene state (envWindow env)
    GL.flush
    GL.preservingMatrix $ do
      if (stateCursor state) then (drawTile (stateTexs state) (stateCursorX state) (stateCursorY state) 8) else (drawTile (stateTexs state) 200 100 8)
    let tile = findTileType (stateGrid state) (stateCursorX state) (stateCursorY state)
    --if (tile == 5) then do
      --GL.preservingMatrix $ do
      --  let x = 0.0::GL.GLfloat
      --  let y = 0.0::GL.GLfloat
      --  let z = 0.0::GL.GLfloat
      --  GL.translate $ GL.Vector3 x y z
      --  font <- createTextureFont "data/fonts/amatic/AmaticSC-Regular.ttf"
      --  setFontFaceSize font 96 96
      --  renderFont font "A Bridge Far Away..." Front
      --GL.flush
    --else do
      --GL.preservingMatrix $ do
       -- let x = -50.0::GL.GLfloat
        --let y = 0.0::GL.GLfloat
       -- let z = 1::GL.GLfloat
       -- GL.translate $ GL.Vector3 x y z
       -- font <- createTextureFont "data/fonts/amatic/AmaticSC-Regular.ttf"
        --setFontFaceSize font 24 24
        --renderFont font "A Bridge Far Away..." Front
    GL.flush

draw _     = do
  state <- get
  liftIO $ do
    drawText 1 95 "A Bridge Far Away..."

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
        env   <- ask
        when (k == GLFW.Key'Escape) $ do
          liftIO $ saveMap (stateGrid state)
          liftIO $ GLFW.setWindowShouldClose window True
        when ((k == GLFW.Key'Space) && ((stateGame state) == SWorld)) $
          modify $ \s -> s { stateGame = SMenu }
        when ((k == GLFW.Key'C) && ((stateGame state) == SMenu)) $
          modify $ \s -> s { stateGame = SWorld }
        when ((k == GLFW.Key'Up) && ((stateGame state) == SWorld) && ((stateCursorY state) <= (gridh-2))) $
          modify $ \s -> s { stateCursorY = ((stateCursorY state)+1) }
        when ((k == GLFW.Key'Down) && ((stateGame state) == SWorld) && ((stateCursorY state) > 0)) $
          modify $ \s -> s { stateCursorY = ((stateCursorY state)-1) }
        when ((k == GLFW.Key'Left) && ((stateGame state) == SWorld) && ((stateCursorX state) > 0)) $
          modify $ \s -> s { stateCursorX = ((stateCursorX state)-1) }
        when ((k == GLFW.Key'Right) && ((stateGame state) == SWorld) && ((stateCursorX state) <= (gridw-2))) $
          modify $ \s -> s { stateCursorX = ((stateCursorX state)+1) }

    (EventFramebufferSize _ width height) -> do
      adjustWindow
    (EventWindowResize win w h) -> do
      modify $ \s -> s { stateScreenHeight = h 
                      , stateScreenWidth  = w
                      }
      adjustWindow

adjustWindow :: Game ()
adjustWindow = do
  state <- get
  let width  = stateScreenWidth  state
      height = stateScreenHeight state

  let pos   = GL.Position 0 0
      size  = GL.Size (fromIntegral width) (fromIntegral height)
      h     = fromIntegral height / fromIntegral width :: Double
  liftIO $ do
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GLU.perspective 45 (1/h) (0.1) 500
    GL.matrixMode GL.$= GL.Modelview 0
    GL.loadIdentity

-- GLFW window opening
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h title f = do
  GLFW.setErrorCallback $ Just glfwErrorCallback
  True <- GLFW.init
  --GLFW.windowHint $ GLFW.WindowHint'Resizable False
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
reshapeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
reshapeCallback tc win w h = atomically $ writeTQueue tc $ EventWindowResize win w h
