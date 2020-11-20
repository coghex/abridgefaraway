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
  runLoadLoop env LoadCmdNULL TStop

runLoadLoop ∷ Env → LoadCmd → TState → IO ()
runLoadLoop env _ TStop = do
  let timerChan = envLTimerChan env
      lCmdChan  = envLCmdChan   env
  tsnew ← atomically $ readChan timerChan
  firstCmd ← atomically $ readChan lCmdChan
  runLoadLoop env firstCmd tsnew
runLoadLoop env cmd TStart = do
  start ← getCurrentTime
  let timerChan = envLTimerChan env
      eventQ    = envEventsChan env
  timerstate ← atomically $ tryReadChan timerChan
  tsnew ← case (timerstate) of 
    Nothing → return TStart
    Just x  → return x
  -- logic
  ret ← case cmd of
    LoadCmdWin ls → do
      let newDS = loadDrawState ls
          currWin = (luaWindows ls) !! (luaCurrWin ls)
      atomically $ writeQueue eventQ $ EventLoadedLuaState newDS
      return "success"
    LoadCmdInit  → runLoadLoop env LoadCmdNULL TPause ≫ return "load loop exiting"
    LoadCmdNULL  → return "NULL load command"
  --atomically $ writeQueue eventQ $ EventLogDebug $ "load command returned: " ⧺ ret
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  runLoadLoop env LoadCmdNULL TPause
runLoadLoop env _ TPause = do
  let lCmdChan = envLCmdChan env
  newCmd ← atomically $ readChan lCmdChan
  runLoadLoop env newCmd TStart
runLoadLoop _   _   TNULL  = return ()
