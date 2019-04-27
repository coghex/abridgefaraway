module ABFA.Input where
-- the inputs from mouse and keyboard are processed

import Control.Monad.RWS.Strict (liftIO, asks, ask, gets, get, modify)
import Control.Monad (when)
import Data.Char (toUpper)
import qualified GLUtil.ABFA as GLFW
import ABFA.Game
import ABFA.State
import ABFA.Data
import ABFA.Event
import ABFA.World
import ABFA.Shell

-- case function for all of the keys
evalKey :: GLFW.Window -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> Game ()
evalKey window k ks mk = do
  state <- get
  env   <- ask
  inpkey <- liftIO $ calcInpKey k mk
  let keylayout = settingKeyLayout (stateSettings state)
  let gs        = stateGame state
  -- quits from the menu
  when ((gs == SMenu) && (keyCheck keylayout k "ESC")) $ do
    liftIO $ GLFW.closeGLFW window
  -- exits when in world view, in future should save game
  when ((gs == SWorld) && (keyCheck keylayout k "ESC")) $ do
    liftIO $ GLFW.closeGLFW window
  -- creates world from the menu
  when ((gs == SMenu) && (keyCheck keylayout k "C")) $ do
    liftIO $ loadedCallback (envEventsChan env) SLoadWorld
    let newstate = initGoodWorld state
    -- let the timers know that we have generated a new state
    liftIO $ atomically $ writeChan (envStateChan2 env) newstate
    liftIO $ atomically $ writeChan (envStateChan4 env) newstate
    modify $ \s -> newstate
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
  -- opens a lua shell
  when ((gs /= SShell) && (keyCheck keylayout k "`")) $ do
    liftIO $ loadedCallback (envEventsChan env) SShell
  -- closes the shell
  when ((gs == SShell) && ((keyCheck keylayout k "`") || (keyCheck keylayout k "ESC"))) $ do
    liftIO $ loadedCallback (envEventsChan env) $ stateGamePrev state
  -- deletes stuff
  when ((gs == SShell) && (keyCheck keylayout k "DEL")) $ do
    modify $ \s -> s { stateShellInput = inputDelete (stateShellInput state) }
  -- types a space
  when ((gs == SShell) && (keyCheck keylayout k "SPC")) $ do
    modify $ \s -> s { stateShellInput = (stateShellInput state) ++ " " }
  -- runs a lua command
  when ((gs == SShell) && (keyCheck keylayout k "RET")) $ do
    outbuff <- liftIO $ execShell (stateLua state) (stateShellInput state)
    let newbuff = (outbuff) : (" %  " ++ (stateShellInput state)) : (tail (stateShellBuff state))
    modify $ \s -> s { stateShellBuff  = " % " : newbuff
                     , stateShellInput = "" }
  -- reads the users keyboard
  when ((gs == SShell) && (not ((keyCheck keylayout k "`") || (keyCheck keylayout k "DEL") || (keyCheck keylayout k "ESC") || (keyCheck keylayout k "SPC") || (keyCheck keylayout k "RET")))) $ do
    let newinp = (stateShellInput state) ++ inpkey
    modify $ \s -> s { stateShellInput = newinp }
  
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
