module Epiklesis.World where
-- data and functions for a
-- world object in the lua state

data World = WorldNULL | World { worldSize ∷ (Int,Int)
                               , worldGrid ∷ [[Int]]
                               , worldTex  ∷ String
                               } deriving (Show, Eq)
