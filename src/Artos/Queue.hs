module Artos.Queue where
-- an event queue is defined
import qualified Control.Concurrent.STM as STM
import Artos.Data
import qualified Paracletus.Oblatum.GLFW as GLFW
import Paracletus.Data
import Epiklesis.Data
import Epiklesis.World
-- type synonym for ease of use
type Queue = STM.TQueue
type TChan = STM.TChan
-- this is a placeholder
data Event = EventError !GLFW.Error !String
           | EventLogDebug !String
           | EventLoaded
           | EventLoadedLuaState !DrawState
           | EventLua !LuaCmd
           | EventUpdateSegs !SegUpdateData
           | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys-- deriving (Show)
-- these functions act as synonyms
-- to the STM library
newQueue ∷ IO (Queue Event)
newQueue = STM.newTQueueIO
newCmdQueue ∷ IO (Queue LoadCmd)
newCmdQueue = STM.newTQueueIO
writeQueue ∷ Queue α → α → STM.STM ()
writeQueue = STM.writeTQueue
tryReadQueue ∷ STM.TQueue α → STM.STM (Maybe α)
tryReadQueue = STM.tryReadTQueue
newTChan ∷ IO (TChan a)
newTChan = STM.atomically $ STM.newTChan
readChan     :: STM.TChan a -> STM.STM a
readChan     = STM.readTChan
tryReadChan  :: STM.TChan a -> STM.STM (Maybe a)
tryReadChan  = STM.tryReadTChan
writeChan    :: STM.TChan a -> a -> STM.STM ()
writeChan    = STM.writeTChan
