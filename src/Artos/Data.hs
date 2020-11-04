module Artos.Data where
-- various data structures for threads
import Prelude()
import UPrelude

-- timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)
