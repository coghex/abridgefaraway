module Main where
-- test routine for the artos
-- library, an exception handler
-- and thread helper for a generic
-- monadic action, meant to work
-- with anamnesis and paracletus,
-- the monad and the action
-- respectively. in that context it 
-- runs a graphics engine in the
-- context of a general application.
-- abstract tests, for threads and
-- queues, are in their own modules.
import Artos
import Anamnesis.Init
import Paracletus
-- runs a monadic action in IO,
-- runMProg applies the monadic
-- action, checkStatus handles the
-- evaluation results, runProg is a
-- action to be run, the only ioref
-- allowed is the program status.
main ∷ IO ()
main = runAnamnesis checkStatus (runParacletus)
