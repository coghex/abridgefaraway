module Artos.Queue where
-- an event queue is defined
import qualified Control.Concurrent.STM as STM
-- type synonym for ease of use
type Queue = STM.TQueue
-- this is a placeholder
data Event = Event1 | Event2 deriving (Eq, Read, Show)
-- these functions act as synonyms
-- to the STM library
newQueue ∷ IO (Queue Event)
newQueue = STM.newTQueueIO
writeQueue ∷ Queue a → a → STM.STM ()
writeQueue = STM.writeTQueue
