{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Paracletus.Vulkan.Tile where
-- vulkan specific translations from
-- the tiles array to a bunch of vertices
import Prelude()
import UPrelude
import Graphics.Vulkan.Core_1_0
import Numeric.DataFrame
import Numeric.Dimensions
import Paracletus.Data
import Paracletus.Vulkan.Atlas
import Paracletus.Vulkan.Vertex

vertices ∷ [GTile] → DataFrame Vertex '[XN 0]
vertices tiles = fromList $ combineVertices tiles
  where vertsqs = [ S $ Vertex (vec3 (-1) (-1) 0) (vec3 1 0 0) (vec3 0 1 0.1)
                  , S $ Vertex (vec3   1  (-1) 0) (vec3 0 1 0) (vec3 1 1 0.1)
                  , S $ Vertex (vec3   1    1  0) (vec3 0 0 1) (vec3 1 0 0.1)
                  , S $ Vertex (vec3 (-1)   1  0) (vec3 1 1 1) (vec3 0 0 0.1) ]
        combineVertices [] = []
        combineVertices (tile:tiles) = withTC (indexAtlas 0 0 sx sy) (withTC (+ vec3 ax ay t) (withPos (+ vec4 x y 0 0) vertsqs)) ⧺ combineVertices tiles
          where (x',y') = tPos tile
                ( x, y) = (2*fromIntegral x', 2*fromIntegral y')
                (ax', ay') = tInd tile
                ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
                (sx, sy) = tSize tile
                t = fromIntegral $ tT tile
        withPos f = map (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
        withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })

indices ∷ [GTile] → DataFrame Word32 '[XN 3]
indices tiles = atLeastThree $ fromList $ (combineIndices tiles)
combineIndices ∷ ∀ a. (Num a) ⇒ [GTile] → [a]
combineIndices []           = []
combineIndices (tile:tiles) = oneRectIndices ⧺ (map (+4) (combineIndices tiles))
  where oneRectIndices = [0,3,2,2,1,0]
