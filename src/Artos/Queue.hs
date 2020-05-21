module Artos.Queue where
-- an event queue is defined
import qualified Control.Concurrent.STM as STM
import qualified Graphics.UI.GLFW as GLFW
-- type synonym for ease of use
type Queue = STM.TQueue
-- this is a placeholder
data Event = EventError !String
           | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | Event deriving (Show)
-- these functions act as synonyms
-- to the STM library
newQueue ∷ IO (Queue Event)
newQueue = STM.newTQueueIO
writeQueue ∷ Queue a → a → STM.STM ()
writeQueue = STM.writeTQueue
