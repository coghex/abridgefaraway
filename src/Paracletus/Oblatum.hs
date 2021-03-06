{-# LANGUAGE Strict #-}
module Paracletus.Oblatum where
-- the input side of GLFW is handled
import Prelude()
import UPrelude
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless, forever)
import Control.Monad.State.Class (gets, modify)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Anamnesis
import Anamnesis.Data
import Anamnesis.Util
import Artos.Except
import Artos.Var
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Oblatum.Callback
import Paracletus.Oblatum.Data
import Paracletus.Oblatum.Event
import Paracletus.Oblatum.GLFW (WindowHint(..),ClientAPI(..))
import Paracletus.Oblatum.Input
import qualified Paracletus.Oblatum.GLFW as GLFW

initGLFWWindow ∷ Int → Int → String → TVar Bool → Anamnesis ε σ GLFW.Window
initGLFWWindow w h n windowSizeChanged = do
  env ← ask
  let eventsChan = envEventsChan env
  allocResource
    (\() → liftIO GLFW.terminate ≫ logInfo "terminated glfw")
    (liftIO GLFW.init ⌦ flip unless
      (logExcept GLFWError ExParacletus "failed to init glfw")
    )
  -- this one we can set before create window
  liftIO $ GLFW.setErrorCallback $ Just $ errorCallback eventsChan
  liftIO GLFW.getVersionString ⌦ mapM_ (logInfo ∘ ("glfw version: " ⧺))
  liftIO GLFW.vulkanSupported ⌦ flip unless
    (logExcept GLFWError ExParacletus "glfw does not support vulkan")
  liftIO ∘ GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  liftIO ∘ GLFW.windowHint $ WindowHint'Resizable True
  allocResource
    ( \window → do
        liftIO (GLFW.destroyWindow window)
        logDebug "closed glfw window"
    ) $ do
    mw ← liftIO $ GLFW.createWindow w h n Nothing Nothing
    case mw of
      Nothing → logExcept GLFWError ExParacletus "failed to init GLFW"
      Just window → do
        logDebug "initialized glfw window"
        liftIO $ GLFW.setKeyCallback window $ Just $ keyCallback eventsChan
        liftIO $ GLFW.setMouseButtonCallback window $ Just $ mouseButtonCallback eventsChan
        liftIO $ GLFW.setWindowSizeCallback window $
          Just (\_ _ _ → atomically $ writeTVar windowSizeChanged True)
        return window

loadLoop ∷ GLFW.Window → Anamnesis' ε LoopControl → Anamnesis ε σ Bool
loadLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            stick ← gets sTick
            newtick ← case stick of
                  Just st → return st
                  Nothing → liftIO $ getCurTick
            modify $ \s → s { sTick = Just newtick }
            status ← locally action
            if status ≡ ContinueLoop then go else return False
          else return True


glfwMainLoop ∷ GLFW.Window → Anamnesis' ε LoopControl → Anamnesis ε σ Bool
glfwMainLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            stick ← gets sTick
            newtick ← case stick of
                  Just st → return st
                  Nothing → liftIO $ getCurTick
            modify $ \s → s { sTick = Nothing }
            status ← locally action
            st ← get
            let srec  = not $ (sReload st) ∨ (sRecreate st)
            if srec then do
              sfps ← gets sFPS
              let fps = fst sfps
                  dfps = snd sfps
                  deltafps = 0.1
              liftIO $ whileM_ ((\cur → (cur - (newtick)) < (1.0/fps)) <$> getCurTick) (liftIO (threadDelay 1000))
              if (dfps > 60) then modify $ \s → s { sFPS = (fps-deltafps, dfps) }
              else if (dfps < 60) then modify $ \s → s { sFPS = (min 200.0 (fps+deltafps), dfps) }
              else modify $ \s → s { sFPS = (fps, dfps) }
            else return ()
            if status ≡ ContinueLoop then go else return False
            else return True

-- loop for the monad
whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ p f = do
  x <- p
  when x $ do f >> whileM_ p f

-- gets time in ms
getCurTick :: IO Double
getCurTick = do
  tickUCT <- getCurrentTime
  return (fromIntegral (round $ utctDayTime tickUCT * 1000000 :: Integer) / 1000000.0 :: Double)

drawLoop ∷ GLFW.Window → Anamnesis' ε Bool → Anamnesis ε σ Bool
drawLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            status ← locally action
            if status ≡ True then go else return False
          else return True

-- runs glfw in the main thread
-- waiting for events every second
glfwWaitEventsMeanwhile ∷ Anamnesis' ε () → Anamnesis ε σ ()
glfwWaitEventsMeanwhile action = occupyThreadAndFork (liftIO $ forever $ GLFW.waitEventsTimeout 1.0) action

-- glfw will wait when minimized
-- so as not to steal input
glfwWaitMinimized ∷ GLFW.Window → Anamnesis ε σ ()
glfwWaitMinimized win = liftIO go where
  go = do
    (x,y) ← GLFW.getFramebufferSize win
    GLFW.waitEvents
    when (x ≡ 0 ∧ y ≡ 0) go

-- this will be run every frame to handle input
processInput ∷ Anamnesis ε σ ()
processInput = do
    st ← get
    let ls      = luaSt st
        windows = luaWindows (ls)
        win     = windows !! (luaCurrWin ls)
        is      = inputState st
    case (winType win) of
      WinTypeGame → if (mouse3 is) then moveCamWithMouse
                    else if ((keyUp is) ∨ (keyLeft is) ∨ (keyDown is) ∨ (keyRight is) ∨ ((((abs (fst (keyAccel is))) > 0.0) ∨ (abs (snd (keyAccel is)) > 0.0)))) then moveCamWithKeys
                    else return ()
      WinTypeMenu → if (mouse1 is) then
                      if ((sliderPressed is) > 0) then do
                        moveSliderWithMouse (sliderPressed is)
                      --else if ((selectedBox is) > 0) then do
                      --  typeInBox (selectedBox is)
                      else return ()
                    else return ()
      WinTypeNULL → return ()

initInputState ∷ InputState
initInputState = InputState { mouse1      = False
                            , mouse1Cache = (0.0,0.0)
                            , mouse2      = False
                            , mouse2Cache = (0.0,0.0)
                            , mouse3      = False
                            , mouse3Cache = (0.0,0.0)
                            , isElems     = []
                            , inpCap      = False
                            , keyUp       = False
                            , keyLeft     = False
                            , keyDown     = False
                            , keyRight    = False
                            , keyAccel    = (0.0,0.0) }
