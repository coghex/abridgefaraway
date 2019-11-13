module ABFA.Input where
-- the inputs from mouse and keyboard are processed

import Control.Monad.RWS.Strict (liftIO, asks, ask, gets, get, modify)
import Control.Monad (when)
import Data.Char (toUpper)
import qualified GLUtil.ABFA as GLFW
import qualified Data.ByteString.Lazy as BS
import ABFA.Game
import ABFA.State
import ABFA.Data
import ABFA.Event
import ABFA.World
import ABFA.Shell
import ABFA.Settings
import ABFA.Zone
import ABFA.Map

-- case function for all of the keys
evalKey :: GLFW.Window -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> Game ()
evalKey window k ks mk = do
  state <- get
  env   <- ask
  inpkey <- liftIO $ calcInpKey k mk
  let settings  = stateSettings state
  let keylayout = settingKeyLayout settings
  let gs        = stateGame state
  -- quits from the menu
  when ((gs == SMenu) && (keyCheck keylayout k "ESC")) $ do
    liftIO $ GLFW.closeGLFW window
    return ()
  -- exits when in world view, in future should save game
  when ((gs == SWorld) && (keyCheck keylayout k "ESC")) $ do
    liftIO $ GLFW.closeGLFW window
    return ()
  -- returns to menu from zone view
  when ((gs == SZone) && (keyCheck keylayout k "ESC")) $ do
    liftIO $ loadedCallback (envEventsChan env) SMenu
    return ()
  -- creates world from the menu
  when ((gs == SMenu) && (keyCheck keylayout k "C")) $ do
    liftIO $ loadedCallback (envEventsChan env) SLoadWorld
    let newstate = initGoodWorld state
    -- let the timers know that we have generated a new state
    liftIO $ atomically $ writeChan (envStateChan2 env) newstate
    liftIO $ atomically $ writeChan (envStateChan4 env) newstate
    modify $ \s -> newstate
    return ()
  -- regenerates the world from the world screen
  when ((gs == SWorld) && (keyCheck keylayout k "R")) $ do
    -- stop the timers
    liftIO $ atomically $ writeChan (envWTimerChan env) TStop
    liftIO $ atomically $ writeChan (envATimerChan env) TStop
    -- ensure empty channels
    liftIO $ emptyChan (envStateChan1 env)
    liftIO $ emptyChan (envStateChan2 env)
    liftIO $ emptyChan (envStateChan3 env)
    liftIO $ emptyChan (envStateChan4 env)
    -- create new world
    let newstate = initWorldWithCheck state
    -- just to be sure, empty the channels again
    liftIO $ emptyChan (envStateChan1 env)
    liftIO $ emptyChan (envStateChan2 env)
    liftIO $ emptyChan (envStateChan3 env)
    liftIO $ emptyChan (envStateChan4 env)
    -- alert the timers of the new state
    liftIO $ atomically $ writeChan (envStateChan2 env) newstate
    liftIO $ atomically $ writeChan (envStateChan4 env) newstate
    modify $ \s -> newstate
    return ()
  -- moves the cursor orthographically, shift will move 5...
  when ((gs == SWorld) && ((keyCheck keylayout k "LFT") || (keyCheck keylayout k "RGT") || (keyCheck keylayout k "UPP") || (keyCheck keylayout k "DWN"))) $ do
    when ((gs == SWorld) && (keyCheck keylayout k "LFT")) $ do
      let step     = if (GLFW.modifierKeysShift mk) then 5 else 1
      modify $ \s -> s { stateCursor = (moveCursor step (stateCursor state) (settingGridW settings) (settingGridH settings) West) }
    when ((gs == SWorld) && (keyCheck keylayout k "RGT")) $ do
      let step     = if (GLFW.modifierKeysShift mk) then 5 else 1
      modify $ \s -> s { stateCursor = (moveCursor step (stateCursor state) (settingGridW settings) (settingGridH settings) East) }
    when ((gs == SWorld) && (keyCheck keylayout k "UPP")) $ do
      let step     = if (GLFW.modifierKeysShift mk) then 5 else 1
      modify $ \s -> s { stateCursor = (moveCursor step (stateCursor state) (settingGridW settings) (settingGridH settings) North) }
    when ((gs == SWorld) && (keyCheck keylayout k "DWN")) $ do
      let step     = if (GLFW.modifierKeysShift mk) then 5 else 1
      modify $ \s -> s { stateCursor = (moveCursor step (stateCursor state) (settingGridW settings) (settingGridH settings) South) }
  return ()
  -- enters zone state
  when ((gs == SWorld) && (keyCheck keylayout k "RET")) $ do
    let oldzs    = stateZone state
        z        = generateZone state cx     cy
        ze       = generateZone state (cx+1) cy
        zw       = generateZone state (cx-1) cy
        zn       = generateZone state cx     (cy+1)
        zs       = generateZone state cx     (cy-1)
        znw      = generateZone state (cx-1) (cy+1)
        zne      = generateZone state (cx+1) (cy+1)
        zsw      = generateZone state (cx-1) (cy-1)
        zse      = generateZone state (cx+1) (cy-1)
        (cx, cy) = stateCursor state
    liftIO $ loadedCallback (envEventsChan env) SLoadZone
    --iftIO $ print $ "gbs: " ++ (show ((bsToList (cbs (zonechunk z)) 1)))
    modify $ \s -> s { stateZone   = znw:zn:zne:zw:z:ze:zsw:zs:zse:oldzs
    --modify $ \s -> s { stateZone   = z:oldzs
                     , stateEmbark = (cx, cy) }
    return ()
  -- moves the zone camera orthographically around the screen
  when ((gs == SZone) && ((keyCheck keylayout k "CL") || (keyCheck keylayout k "CR") || (keyCheck keylayout k "CU") || (keyCheck keylayout k "CD"))) $ do
    when ((gs == SZone) && (keyCheck keylayout k "CL")) $ do
      let step     = if (GLFW.modifierKeysShift mk) then 5.0 else 1.0
      let newcam   = moveCam DLeft  step (stateZoneCam state)
      modify $ \s -> s { stateZoneCam = newcam }
    when ((gs == SZone) && (keyCheck keylayout k "CR")) $ do
      let step     = if (GLFW.modifierKeysShift mk) then 5.0 else 1.0
      let newcam   = moveCam DRight step (stateZoneCam state)
      modify $ \s -> s { stateZoneCam = newcam }
    when ((gs == SZone) && (keyCheck keylayout k "CU")) $ do
      let step     = if (GLFW.modifierKeysShift mk) then 5.0 else 1.0
      let newcam   = moveCam DUp    step (stateZoneCam state)
      modify $ \s -> s { stateZoneCam = newcam }
    when ((gs == SZone) && (keyCheck keylayout k "CD")) $ do
      let step     = if (GLFW.modifierKeysShift mk) then 5.0 else 1.0
      let newcam   = moveCam DDown  step (stateZoneCam state)
      modify $ \s -> s { stateZoneCam = newcam }
    return ()
  -- opens a lua shell
  when ((gs /= SShell) && (keyCheck keylayout k "`")) $ do
    liftIO $ loadedCallback (envEventsChan env) SShell
    return ()
  -- closes the shell
  when ((gs == SShell) && ((keyCheck keylayout k "`") || (keyCheck keylayout k "ESC"))) $ do
    liftIO $ loadedCallback (envEventsChan env) $ stateGamePrev state
    return ()
  -- deletes stuff
  when ((gs == SShell) && (keyCheck keylayout k "DEL")) $ do
    modify $ \s -> s { stateShellInput = inputDelete (stateShellInput state) }
    return ()
  -- types a space
  when ((gs == SShell) && (keyCheck keylayout k "SPC")) $ do
    modify $ \s -> s { stateShellInput = (stateShellInput state) ++ " " }
    return ()
  -- runs a lua command
  when ((gs == SShell) && (keyCheck keylayout k "RET")) $ do
    outbuff <- liftIO $ execShell (stateLua state) (stateShellInput state)
    newsets <- liftIO $ reimportSettings (stateLua state) "mods/config/"
    let win     = envWindow env
        w       = settingScreenW newsets
        h       = settingScreenH newsets
        newbuff = (outbuff) : (" %  " ++ (stateShellInput state)) : (tail (stateShellBuff state))
    liftIO $ reshapeCallback (envEventsChan env) win w h
    modify $ \s -> s { stateSettings   = newsets
                     , stateShellBuff  = " % " : newbuff
                     , stateShellInput = "" }
    return ()
  -- reads the users keyboard
  when ((gs == SShell) && (not ((keyCheck keylayout k "`") || (keyCheck keylayout k "DEL") || (keyCheck keylayout k "ESC") || (keyCheck keylayout k "SPC") || (keyCheck keylayout k "RET")))) $ do
    let newinp = (stateShellInput state) ++ inpkey
    modify $ \s -> s { stateShellInput = newinp }
    return ()

-- case function for when a key repeats
evalKeyHeld :: GLFW.Window -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> Game ()
evalKeyHeld window k ks mk = do
  state <- get
  env   <- ask
  inpkey <- liftIO $ calcInpKey k mk
  let settings  = stateSettings state
  let keylayout = settingKeyLayout settings
  let gs        = stateGame state
  -- moves orthographically when the movement keys are held
  when ((gs == SWorld) && ((keyCheck keylayout k "LFT") || (keyCheck keylayout k "RGT") || (keyCheck keylayout k "UPP") || (keyCheck keylayout k "DWN"))) $ do
    when ((gs == SWorld) && (keyCheck keylayout k "LFT")) $ do
      let step = if (GLFW.modifierKeysShift mk) then 5 else 1
      modify $ \s -> s { stateCursor = (moveCursor step (stateCursor state) (settingGridW settings) (settingGridH settings) West) }
    when ((gs == SWorld) && (keyCheck keylayout k "RGT")) $ do
      let step = if (GLFW.modifierKeysShift mk) then 5 else 1
      modify $ \s -> s { stateCursor = (moveCursor step (stateCursor state) (settingGridW settings) (settingGridH settings) East) }
    when ((gs == SWorld) && (keyCheck keylayout k "UPP")) $ do
      let step = if (GLFW.modifierKeysShift mk) then 5 else 1
      modify $ \s -> s { stateCursor = (moveCursor step (stateCursor state) (settingGridW settings) (settingGridH settings) North) }
    when ((gs == SWorld) && (keyCheck keylayout k "DWN")) $ do
      let step = if (GLFW.modifierKeysShift mk) then 5 else 1
      modify $ \s -> s { stateCursor = (moveCursor step (stateCursor state) (settingGridW settings) (settingGridH settings) South) }
    return ()
  -- moves orthographically when the camera keys are held
  when ((gs == SZone) && ((keyCheck keylayout k "CL") || (keyCheck keylayout k "CR") || (keyCheck keylayout k "CU") || (keyCheck keylayout k "CD"))) $ do
    when ((gs == SZone) && (keyCheck keylayout k "CL")) $ do
      let step   = if (GLFW.modifierKeysShift mk) then 5.0 else 1.0
      let newcam = moveCam DLeft  step (stateZoneCam state)
      modify $ \s -> s { stateZoneCam = newcam }
    when ((gs == SZone) && (keyCheck keylayout k "CR")) $ do
      let step   = if (GLFW.modifierKeysShift mk) then 5.0 else 1.0
      let newcam = moveCam DRight step (stateZoneCam state)
      modify $ \s -> s { stateZoneCam = newcam }
    when ((gs == SZone) && (keyCheck keylayout k "CU")) $ do
      let step   = if (GLFW.modifierKeysShift mk) then 5.0 else 1.0
      let newcam = moveCam DUp    step (stateZoneCam state)
      modify $ \s -> s { stateZoneCam = newcam }
    when ((gs == SZone) && (keyCheck keylayout k "CD")) $ do
      let step   = if (GLFW.modifierKeysShift mk) then 5.0 else 1.0
      let newcam = moveCam DDown  step (stateZoneCam state)
      modify $ \s -> s { stateZoneCam = newcam }
    return ()

-- checks key with settings
keyCheck :: KeyLayout -> GLFW.Key -> String -> Bool
keyCheck keylayout k str = (k == (GLFW.getGLFWKey nk))
  where nk = getKey keylayout str

-- retrieves user key setting
getKey :: KeyLayout -> String -> String
getKey keylayout "ESC" = keyESC keylayout
getKey keylayout "RET" = keyRET keylayout
getKey keylayout "DEL" = keyDEL keylayout
getKey keylayout "SPC" = keySPC keylayout
getKey keylayout "C"   = keyC   keylayout
getKey keylayout "R"   = keyR   keylayout
getKey keylayout "`"   = keySh  keylayout
getKey keylayout "LFT" = keyLFT keylayout
getKey keylayout "RGT" = keyRGT keylayout
getKey keylayout "UPP" = keyUPP keylayout
getKey keylayout "DWN" = keyDWN keylayout
getKey keylayout "CL"  = keyCL  keylayout
getKey keylayout "CR"  = keyCR  keylayout
getKey keylayout "CU"  = keyCU  keylayout
getKey keylayout "CD"  = keyCD  keylayout
getKey keylayout _     = "NULL"

-- returns the char for a glkey
calcInpKey :: GLFW.Key -> GLFW.ModifierKeys -> IO String
calcInpKey k mk = do
  inp <- GLFW.getKeyStr k
  case (inp) of
    Just str -> return $ applyMod mk str
    Nothing  -> return "" 

-- makes letters capital when shift is held down
applyMod :: GLFW.ModifierKeys -> String -> String
applyMod mk str = if (GLFW.modifierKeysShift mk) then map myUpper str
                  else str
  
-- toUpper wrapper to add numeric keys
myUpper :: Char -> Char
myUpper '1'  = '!'
myUpper '2'  = '@'
myUpper '3'  = '#'
myUpper '4'  = '$'
myUpper '5'  = '%'
myUpper '6'  = '^'
myUpper '7'  = '&'
myUpper '8'  = '*'
myUpper '9'  = '('
myUpper '0'  = ')'
myUpper '-'  = '_'
myUpper '='  = '+'
myUpper '['  = '{'
myUpper ']'  = '}'
myUpper '\\' = '|'
myUpper ';'  = ':'
myUpper '\'' = '"'
myUpper ','  = '<'
myUpper '.'  = '>'
myUpper '/'  = '?'
myUpper c    = toUpper c


-- evaluates mouse input
evalMouse :: GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> Game ()
evalMouse win mb mbs mk = do
  state <- get
  when (((stateGame state) == SWorld) && (mb == GLFW.mousebutt1)) $ do
    (x, y) <- liftIO $ GLFW.getCursorPos win
    liftIO . print $ "x: " ++ (show x) ++ " y: " ++ (show y)

-- evaluates mouse scrolling
evalScroll :: GLFW.Window -> Double -> Double -> Game ()
evalScroll win x y = do
  state <- get
  when (((stateGame state) == SZone)) $ do
    let zoom = (stateZoom state) + (10*(realToFrac y))
    modify $ \s -> s { stateZoom = zoom }

