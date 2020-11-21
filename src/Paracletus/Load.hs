{-# LANGUAGE Strict #-}
module Paracletus.Load where
-- we define the thread that helps
-- recreate the swapchain
import Prelude()
import UPrelude
import Artos.Data
import Artos.Var
import Artos.Queue
import Anamnesis.Data
import Epiklesis.Data
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

loadParacVulkan ∷ Env → IO ()
loadParacVulkan env = do
  runLoadLoop env TStop

runLoadLoop ∷ Env → TState → IO ()
runLoadLoop env TStop = do
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
runLoadLoop env TPause = return ()
runLoadLoop _   TNULL  = return ()

processCommands ∷ Env → IO ()
processCommands env = do
    cmd ← atomically $ tryReadQueue $ envLCmdChan env
    case cmd of
      Just cmd → do
        ret ← processCommand env cmd
        --atomically $ writeQueue eventQ $ EventLogDebug $ "load command returned: " ⧺ ret
        processCommands env
      Nothing → return ()
processCommand ∷ Env → LoadCmd → IO String 
processCommand env cmd = do
  ret ← case cmd of
    LoadCmdWin ls → do
      let newDS = loadDrawState ls
          currWin = (luaWindows ls) !! (luaCurrWin ls)
      atomically $ writeQueue (envEventsChan env) $ EventLoadedLuaState newDS
      return "success"
    LoadCmdNULL  → return "NULL load command"
  return ret

