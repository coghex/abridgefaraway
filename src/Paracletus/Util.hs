module Paracletus.Util where
-- some utility functions that
-- dont make sense elsewhere
import Prelude()
import UPrelude
import GHC.Stack
import Artos.Except
import Anamnesis
-- this will kill everything as it
-- prints, use logError to just print
logExcept ∷ (Show res, Eq res) ⇒ HasCallStack ⇒ res → String → Anamnesis r e s a
logExcept res msg = throwError $ AExcept (Just res) (msg ⧺ "\n" ⧺ prettyCallStack callStack) "anamnesis"
