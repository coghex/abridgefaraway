module Artos.Data where
-- various data structures for threads
import Prelude()
import UPrelude
import Epiklesis.Data
import Paracletus.Data

-- timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)

-- cmds that can be asked of
-- the loading thread
data LoadCmd = LoadCmdVerts DrawState
             | LoadCmdWin LuaState
             | LoadCmdWorld LuaState
             | LoadCmdNULL
