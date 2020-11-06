module Epiklesis.World where
-- data and functions for a
-- world object in the lua state
-- zones are indexed from the middle
-- and go negative, segments are
-- indexed from bottom left to top right
-- and dont contain the index in the struct

data World = WorldNULL | World { worldZone ∷ [Zone]
                               , worldTex  ∷ String
                               } deriving (Show, Eq)
data Zone = ZoneNULL | Zone { zoneIndex ∷ (Int,Int)
                            , zoneSegs  ∷ [[Segment]]
                            } deriving (Show, Eq)
data Segment = SegmentNULL | Segment { segGrid ∷ [[Tile]]
                                     } deriving (Show, Eq)
data Tile = TileNULL | Tile { tileCont ∷ Int
                            , tileType ∷ Int
                            } deriving (Show, Eq)
