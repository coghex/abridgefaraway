module Artos.Queue where
-- an event queue is defined
import qualified Control.Concurrent.STM as STM
import qualified Paracletus.Oblatum.GLFW as GLFW
-- type synonym for ease of use
type Queue = STM.TQueue
-- this is a placeholder
data Event = EventError !GLFW.Error !String
           | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys deriving (Show)
-- these functions act as synonyms
-- to the STM library
newQueue ∷ IO (Queue Event)
newQueue = STM.newTQueueIO
writeQueue ∷ Queue α → α → STM.STM ()
writeQueue = STM.writeTQueue
tryReadQueue ∷ STM.TQueue α → STM.STM (Maybe α)
tryReadQueue = STM.tryReadTQueue
