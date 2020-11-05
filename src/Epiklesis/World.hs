module Epiklesis.World where
-- data and functions for a
-- world object in the lua state

data World = WorldNULL | World { worldSize ∷ (Int,Int)
                               , worldSegments ∷ [[WorldSeg]]
                               , worldTex  ∷ String
                               } deriving (Show, Eq)
data WorldSeg = WorldSegNULL | WorldSeg { worldGrid ∷ [[Tile]] } deriving (Show, Eq)
data Tile = TileNULL | Tile { tileCont ∷ Int
                            , tileType ∷ Int
                            } deriving (Show, Eq)
