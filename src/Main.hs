module Main (main) where

import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad (unless, when, void)
import Control.Monad.RWS.Strict (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Text.PrettyPrint

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW

-- enviornmental variables
data Env = Env
  { envEventsChan    :: TQueue Event
  , envWindow        :: !GLFW.Window
  }

-- current state
data State = State
  { stateWindowWidth      :: !Int
  , stateWindowHeight     :: !Int
  , stateMouseDown        :: !Bool
  , stateDragging         :: !Bool
  , stateDragStartX       :: !Double
  , stateDragStartY       :: !Double
  }

-- Game type
type Game = RWST Env () State IO

-- Events that we can process
data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

-- main function
main :: IO ()
main = do
  let screenw = 1600
  let screenh = 1200

  -- event queue
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  -- create window
  withWindow screenw screenh "A Bridge Far Away..." $ \win -> do
    GLFW.setErrorCallback                             $ Just $ errorCallback           eventsChan
    GLFW.setWindowPosCallback                     win $ Just $ windowPosCallback       eventsChan
    GLFW.setWindowSizeCallback                    win $ Just $ windowSizeCallback      eventsChan
    GLFW.setWindowCloseCallback                   win $ Just $ windowCloseCallback     eventsChan
    GLFW.setWindowRefreshCallback                 win $ Just $ windowRefreshCallback   eventsChan
    GLFW.setWindowFocusCallback                   win $ Just $ windowFocusCallback     eventsChan
    GLFW.setWindowIconifyCallback                 win $ Just $ windowIconifyCallback   eventsChan
    GLFW.setFramebufferSizeCallback               win $ Just $ framebufferSizeCallback eventsChan
    GLFW.setMouseButtonCallback                   win $ Just $ mouseButtonCallback     eventsChan
    GLFW.setCursorPosCallback                     win $ Just $ cursorPosCallback       eventsChan
    GLFW.setCursorEnterCallback                   win $ Just $ cursorEnterCallback     eventsChan
    GLFW.setScrollCallback                        win $ Just $ scrollCallback          eventsChan
    GLFW.setKeyCallback                           win $ Just $ keyCallback             eventsChan
    GLFW.setCharCallback                          win $ Just $ charCallback            eventsChan

    GLFW.swapInterval 1

    (fbWidth, fbHeight) <- GLFW.getFramebufferSize win

    let env = Env
          { envEventsChan      = eventsChan
          , envWindow          = win
          }
        state = State
          { stateWindowWidth  = fbWidth
          , stateWindowHeight = fbHeight
          , stateMouseDown    = False
          , stateDragging     = False
          , stateDragStartX   = 0
          , stateDragStartY   = 0
          }
    -- run game
    runGame env state

  putStrLn "ended"


-- create a window using function
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]

-- callback functions
errorCallback           :: TQueue Event -> GLFW.Error -> String                                                          -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                     -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                     -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                   -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                   -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                              -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                     -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                               -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                               -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                               -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys          -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                           -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

-- print instructions and run game
runGame :: Env -> State -> IO ()
runGame env state = do
  printInstructions
  -- this is where we GLinit stuff
  void $ evalRWST (adjustWindow >> run) env state


-- runs game
run :: Game ()
run = do
  win <- asks envWindow
  -- here is where we draw stuff
  liftIO $ do
    GLFW.swapBuffers win
    GL.flush
    GLFW.pollEvents
  processEvents

  -- state <- get
  -- this is where we do GLloop stuff on the state
  q <- liftIO $ GLFW.windowShouldClose win
  unless q run

-- handle events
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
      printEvent "error" [show e, show s]
      win <- asks envWindow
      liftIO $ GLFW.setWindowShouldClose win True

    (EventWindowPos _ x y) ->
      printEvent "window pos" [show x, show y]

    (EventWindowSize _ width height) ->
      printEvent "window size" [show width, show height]

    (EventWindowClose _) ->
      printEvent "window close" []

    (EventWindowRefresh _) ->
      printEvent "window refresh" []

    (EventWindowFocus _ fs) ->
      printEvent "window focus" [show fs]

    (EventWindowIconify _ is) ->
      printEvent "window iconify" [show is]

    (EventFramebufferSize _ width height) -> do
      printEvent "framebuffer size" [show width, show height]
      modify $ \s -> s
        { stateWindowWidth = width
        , stateWindowHeight = height
        }
      adjustWindow

    (EventMouseButton _ mb mbs mk) -> do
      printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
      when (mb == GLFW.MouseButton'1) $ do
        let pressed = mbs == GLFW.MouseButtonState'Pressed
        modify $ \s -> s
          { stateMouseDown = pressed
          }
        unless pressed $
          modify $ \s -> s
            { stateDragging = False
            }

    (EventCursorPos _ x y) -> do
      let x' = round x :: Int
          y' = round y :: Int
      printEvent "cursor pos" [show x', show y']
      state <- get
      when (stateMouseDown state && not (stateDragging state)) $
        put $ state
          { stateDragging        = True
          , stateDragStartX      = x
          , stateDragStartY      = y
          }

    (EventCursorEnter _ cs) ->
      printEvent "cursor enter" [show cs]

    (EventScroll _ x y) -> do
      let x' = round x :: Int
          y' = round y :: Int
      printEvent "scroll" [show x', show y']

    (EventKey win k scancode ks mk) -> do
      printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
      when (ks == GLFW.KeyState'Pressed) $ do
        when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
          liftIO $ GLFW.setWindowShouldClose win True
        when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
          liftIO printInstructions
        when (k == GLFW.Key'I) $
          liftIO $ printInformation win

    (EventChar _ c) ->
      printEvent "char" [show c]


adjustWindow :: Game ()
adjustWindow = do
  state <- get
  let width  = stateWindowWidth  state
      height = stateWindowHeight state
  let pos    = GL.Position 0 0
      size   = GL.Size (fromIntegral width) (fromIntegral height)
  liftIO $ do
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GLU.perspective 45 (fromIntegral width/fromIntegral height) 0.1 500
    GL.flush

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
  x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
  x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
  y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
  y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
  let x0n = if x0 then (-1) else 0
      x1n = if x1 then   1  else 0
      y0n = if y0 then (-1) else 0
      y1n = if y1 then   1  else 0
  return (x0n + x1n, y0n + y1n)

getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)
getJoystickDirections js = do
  maxes <- GLFW.getJoystickAxes js
  return $ case maxes of
    (Just (x:y:_)) -> (-y, x)
    _              -> ( 0, 0)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False


printInstructions :: IO ()
printInstructions =
  putStrLn $ render $
    nest 4 (
      text "-------------------------------------------------------------" $+$
      text "'?': print these instructions"                                 $+$
      text "'i': print info"                                               $+$
      text ""                                                              $+$
      text " -- A Bridge Far Away... -- "                                  $+$
      text "-------------------------------------------------------------"
    )

printInformation :: GLFW.Window -> IO ()
printInformation win = do
  version       <- GLFW.getVersion
  versionString <- GLFW.getVersionString
  monitorInfos  <- runMaybeT getMonitorInfos
  joystickNames <- getJoystickNames
  clientAPI     <- GLFW.getWindowClientAPI              win
  cv0           <- GLFW.getWindowContextVersionMajor    win
  cv1           <- GLFW.getWindowContextVersionMinor    win
  cv2           <- GLFW.getWindowContextVersionRevision win
  robustness    <- GLFW.getWindowContextRobustness      win
  forwardCompat <- GLFW.getWindowOpenGLForwardCompat    win
  debug         <- GLFW.getWindowOpenGLDebugContext     win
  profile       <- GLFW.getWindowOpenGLProfile          win

  putStrLn $ render $
    nest 4 (
      text "------------------------------------------------------------" $+$
      text "GLFW C library:" $+$
      nest 4 (
        text "Version:"        <+> renderVersion version $+$
        text "Version string:" <+> renderVersionString versionString
      ) $+$
      text "Monitors:" $+$
      nest 4 (
        renderMonitorInfos monitorInfos
      ) $+$
      text "Joysticks:" $+$
      nest 4 (
        renderJoystickNames joystickNames
      ) $+$
      text "OpenGL context:" $+$
      nest 4 (
        text "Client API:"            <+> renderClientAPI clientAPI $+$
        text "Version:"               <+> renderContextVersion cv0 cv1 cv2 $+$
        text "Robustness:"            <+> renderContextRobustness robustness $+$
        text "Forward compatibility:" <+> renderForwardCompat forwardCompat $+$
        text "Debug:"                 <+> renderDebug debug $+$
        text "Profile:"               <+> renderProfile profile
      ) $+$
      text "------------------------------------------------------------"
    )
  where
    renderVersion (GLFW.Version v0 v1 v2) =
      text $ intercalate "." $ map show [v0, v1, v2]

    renderVersionString =
      text . show

    renderMonitorInfos =
      maybe (text "(error)") (vcat . map renderMonitorInfo)

    renderMonitorInfo (name, (x,y), (w,h), vms) =
      text (show name) $+$
      nest 4 (
        location <+> size $+$
        fsep (map renderVideoMode vms)
      )
      where
        location = int x <> text "," <> int y
        size     = int w <> text "x" <> int h <> text "mm"

    renderVideoMode (GLFW.VideoMode w h r g b rr) =
      brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz  = int rr <> text "Hz"

    renderJoystickNames pairs =
      vcat $ map (\(js, name) -> text (show js) <+> text (show name)) pairs

    renderContextVersion v0 v1 v2 =
      hcat [int v0, text ".", int v1, text ".", int v2]

    renderClientAPI         = text . show
    renderContextRobustness = text . show
    renderForwardCompat     = text . show
    renderDebug             = text . show
    renderProfile           = text . show

type MonitorInfo = (String, (Int, Int), (Int, Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos =
  getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors

    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
      name <- getMonitorName mon
      vms  <- getVideoModes mon
      MaybeT $ do
        pos <- liftIO $ GLFW.getMonitorPos mon
        size <- liftIO $ GLFW.getMonitorPhysicalSize mon
        return $ Just (name, pos, size, vms)

    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon

    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames :: IO [(GLFW.Joystick, String)]
getJoystickNames =
  catMaybes `fmap` mapM getJoystick joysticks
  where
  getJoystick js =
    fmap (maybe Nothing (\name -> Just (js, name)))
      (GLFW.getJoystickName js)

printEvent :: String -> [String] -> Game ()
printEvent cbname fields =
  liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
  "[mod keys: " ++ keys ++ "]"
  where
  keys = if null xs then "none" else unwords xs
  xs = catMaybes ys
  ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
       , if GLFW.modifierKeysControl mk then Just "control" else Nothing
       , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
       , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
       ]

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x

joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]
