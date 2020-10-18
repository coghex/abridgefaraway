module Main where
-- where the magic happens...
import Artos
import Paracletus
import Paracletus.Data
import Anamnesis.Init
-- runs a monadic action in IO
main ∷ IO ()
main = runAnamnesis checkStatus (runParacletus Vulkan)
