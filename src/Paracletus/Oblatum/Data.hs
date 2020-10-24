module Paracletus.Oblatum.Data where
-- data for the input engine is defined

data InputState = InputState { mouse1 ∷ Bool
                             , mouse1Cache ∷ (Float,Float)
                             , mouse2 ∷ Bool
                             , mouse2Cache ∷ (Float,Float)
                             , mouse3 ∷ Bool
                             , mouse3Cache ∷ (Float,Float)
                             } deriving (Show, Eq)
