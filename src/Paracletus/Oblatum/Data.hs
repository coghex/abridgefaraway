module Paracletus.Oblatum.Data where
-- data for the input engine is defined

data InputState = InputState { mouse3 ∷ Bool
                             , mouseCache ∷ (Float,Float)
                             } deriving (Show, Eq)
