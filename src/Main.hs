module Main where
-- where the magic happens...
import Artos
import Anamnesis.Init
import Paracletus
import Paracletus.Data
-- runs a monadic action in IO
main ∷ IO ()
main = runAnamnesis checkStatus (runParacletus Vulkan)
