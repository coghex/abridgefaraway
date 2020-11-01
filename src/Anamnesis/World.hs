module Anamnesis.World where
import Prelude()
import UPrelude

createWorld ∷ Int → Int → [[Int]]
createWorld w h = take h (repeat (take w (repeat 1)))
