module Paracletus where
-- a graphics layer is chosen,
-- a GLFW instance is begun
import Prelude()
import UPrelude
import GHC.Stack
import Artos.Except
import Artos.Log
import Anamnesis
import Anamnesis.Data
-- a generic action is run in a
-- MProg context, returning ()
runParacletus ∷ Anamnesis ret env state ()
runParacletus = do
  -- simple log functions
  logDebug $ "beginning anamnesis..."
-- *** functions
logExcept ∷ HasCallStack ⇒ AnamnResult → String → Anamnesis r e s ()
logExcept ex msg = throwError $ AExcept (Just ex) (msg ⧺ "\n" ⧺ prettyCallStack callStack) "prog"
