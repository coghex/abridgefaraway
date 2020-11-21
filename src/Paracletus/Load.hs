module Paracletus.Load where
-- we define the thread that helps
-- recreate the swapchain
import Prelude()
import UPrelude
import Artos.Data
import Artos.Var
import Artos.Queue
import Anamnesis.Data
import Anamnesis.World
import Epiklesis.Data
import Epiklesis.Lua
import Paracletus.Data
import Paracletus.Draw
import Control.Concurrent (threadDelay)
import Data.Time.Clock

loadParacletus ∷ Env -> GraphicsLayer → IO ()
loadParacletus env Vulkan   = loadParacVulkan env
loadParacletus env OpenGL   = atomically $ writeQueue ec $ EventLogDebug "not yet implemented"
  where ec = envEventsChan env
loadParacletus env OpenGLES = atomically $ writeQueue ec $ EventLogDebug "not yet implemented"
  where ec = envEventsChan env
loadParacletus env _        = atomically $ writeQueue ec $ EventLogDebug "dont know graphics layer"
  where ec = envEventsChan env

-- loop provides calculations for the main thread
-- so loading new objects doesnt stutter the window
loadParacVulkan ∷ Env → IO ()
loadParacVulkan env = do
  runLoadLoop env TStop

-- load loop runs with a delay so that
-- it can sleep (ghc threads run like that)
runLoadLoop ∷ Env → TState → IO ()
runLoadLoop env TStop = do
  --loop starts almost immediately
  let timerChan = envLTimerChan env
  tsnew ← atomically $ readChan timerChan
  runLoadLoop env tsnew
runLoadLoop env TStart = do
  start ← getCurrentTime
  let timerChan = envLTimerChan env
      eventQ    = envEventsChan env
  timerstate ← atomically $ tryReadChan timerChan
  tsnew ← case (timerstate) of
    Nothing → return TStart
    Just x  → return x
  processCommands env
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  runLoadLoop env tsnew
-- pause not needed for this timer
runLoadLoop env TPause = return ()
runLoadLoop _   TNULL  = return ()

-- command queue processed every tick,
-- logging any errors of all commands
processCommands ∷ Env → IO ()
processCommands env = do
    cmd ← atomically $ tryReadQueue $ envLCmdChan env
    case cmd of
      Just cmd → do
        ret ← processCommand env cmd
        if (ret ≠ "success") then do
          atomically $ writeQueue (envEventsChan env) $ EventLogDebug $ "load command returned: " ⧺ ret
          processCommands env
        else processCommands env
      Nothing → return ()
processCommand ∷ Env → LoadCmd → IO String 
processCommand env cmd = do
  ret ← case cmd of
    -- converts luaState to drawState
    LoadCmdWin ls → do
      let newDS = loadDrawState ls
          currWin = (luaWindows ls) !! (luaCurrWin ls)
      if ((winType currWin) ≡ WinTypeGame) then atomically $ writeQueue (envLCmdChan env) $ LoadCmdWorld ls
      else atomically $ writeQueue (envEventsChan env) $ EventLoadedLuaState newDS
      return "success"
    -- loads a world from a luaState
    -- into a drawState
    LoadCmdWorld ls → do
      let currWin = currentWindow ls
      if ((winType currWin) ≡ WinTypeGame) then do
        let newLS   = loadWorld ls
            newDS   = loadDrawState newLS
        atomically $ writeQueue (envEventsChan env) $ EventLoadedLuaState newDS
        return "success"
      else return "not in game window"
    LoadCmdNULL  → return "NULL load command"
  return ret

