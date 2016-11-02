module Main (main) where

import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Concurrent (forkIO, getNumCapabilities, setNumCapabilities, threadDelay)
import Control.Monad (unless, when, void)
import Control.Monad.RWS.Strict (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Text.PrettyPrint
import System.Random
import qualified GHC.Conc (getNumProcessors)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW

import World
import Util

data Env = Env
    { envEventsChan   :: TQueue Event
    , envWindow       :: !GLFW.Window
    , envGridWidth    :: !Int
    , envGridHeight   :: !Int
    }

data State = State
    { stateGrid :: ![Int]
    , stateTexs :: ![GL.GLuint]
    }

data Event =
      EventError !GLFW.Error !String
    | EventKey   !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys

-- Game type
type Game = RWST Env () State IO

-- runs game on every core
runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities

-- main function
main :: IO ()
main = do
  let screenw = 1600
      screenh = 1200
      gridw   = 120
      gridh   = 90

  eventsChan <- newTQueueIO :: IO (TQueue Event)
  runOnAllCores

  withWindow screenw screenh "A Bridge Far Away..." $ \window -> do
        GLFW.setErrorCallback        $ Just $ errorCallback eventsChan
        GLFW.setKeyCallback   window $ Just $ keyCallback   eventsChan
        GLFW.swapInterval 0
 
  --      init2DGL screenw screenh
        texs <- liftIO $ initTexs window
        let env = Env
                { envEventsChan   = eventsChan
                , envWindow       = window
                , envGridWidth    = gridw
                , envGridHeight   = gridh
                }
            state = State
                { stateGrid = []
                , stateTexs = texs
                }
        void $ evalRWST run env state

run :: Game ()
run = timedLoop $ \lastTick tick -> do
    draw
    window <- asks envWindow
    liftIO $ do
        GLFW.swapBuffers window
        --                         -- GL.finish
        GLFW.pollEvents
        err <- GL.get GL.errors
        mapM_ print err
        whileM_ ((\cur -> (cur - tick) < (1.0 / 60.0)) <$> getCurTick) (threadDelay 100)
    processEvents
    liftIO $ not <$> GLFW.windowShouldClose window

myPoints :: [(GL.GLfloat,GL.GLfloat,GL.GLfloat)]
myPoints = [ (sin (2*pi*k/4), cos (2*pi*k/4), 0) | k <- [1..4] ]

draw :: Game ()
draw = do
  state <- get
  liftIO $ do
    GL.clear [GL.ColorBuffer]
    sceneSetup
    drawTile (stateTexs state) 0 0 1
    --GL.renderPrimitive GL.Points $
    --  mapM_ (\(x, y, z) -> GL.vertex $ GL.Vertex3 x y z) myPoints 
    GL.flush

--init2DGL w h = do
  --GL.matrixMode GL.$= GL.Projection
  --GL.loadIdentity
  --GLU.ortho2D 0.0 (1600) 0.0 (1200)
  --GL.translate (GL.Vector3 60.0 45.0 0.0 :: GL.Vector3 GL.GLfloat)
   
errorCallback :: TQueue Event -> GLFW.Error -> String -> IO ()
errorCallback tc e s            = atomically $ writeTQueue tc $ EventError e s
keyCallback   :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback   tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey win k sc ka mk

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  True <- GLFW.init 
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
  Just window <- GLFW.createWindow w h title Nothing Nothing
  GLFW.makeContextCurrent $ Just window
  f window
  GLFW.setErrorCallback $ Just simpleErrorCallback
  GLFW.destroyWindow window
  GLFW.terminate
  where
    simpleErrorCallback e s = putStrLn $ show e ++ " " ++  show s


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
                when (k == GLFW.Key'Escape) $
                    liftIO $ GLFW.setWindowShouldClose window True

