{-# LANGUAGE Strict #-}
{-# LANGUAGE KindSignatures #-}
module Paracletus.Vulkan.Calc
  ( calcVertices
  , cacheAllGTiles )
where
-- translations from lua state to draw
-- state are defined, should be run
-- outside of parent thread
import Prelude()
import UPrelude
--import Control.Parallel.Strategies (rpar, rseq, runEval)
import Graphics.Vulkan.Core_1_0
import Numeric.DataFrame
import Anamnesis.Data
import Paracletus.Data
import Paracletus.Draw
import Paracletus.Vulkan.Atlas
import Paracletus.Vulkan.Vertex

-- determines dataframes from drawstate,
-- preformance of dataframe creation seems
-- to be negligent, regardless, keep other
-- calcuations to a minimum.
calcVertices ∷ Bool → [GTile] →  (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])
calcVertices til ts = (vertices ts', indices ts')
  where ts' = filter (filterGT til) ts

filterGT ∷ Bool → GTile → Bool
filterGT False (GTileUncached _ _ _ _ _ t _) = not t
filterGT True  (GTileUncached _ _ _ _ _ t _) = t
filterGT False (GTileCached verts) = True
filterGT True  (GTileCached verts) = False

cacheAllGTiles ∷ [GTile] → [GTile]
cacheAllGTiles gts = map cacheGTile gts
cacheGTile ∷ GTile → GTile
cacheGTile (GTileCached d) = GTileCached d
cacheGTile (GTileUncached pos scale ind size t _ moves) = GTileCached (verts)
  where verts = withMove (+ vec3 0 0 m) (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t') (withPos (+ vec4 x0 y0 0 0) (withScale (* vec3 xscale yscale 1) vertsqs))))
        (x',y') = pos
        (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
        (ax', ay') = ind
        ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
        m = case (moves) of
              True  → 1.0
              False → 0.0
        (xscale',yscale') = scale
        (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
        (sx, sy) = size
        t' = fromIntegral $ t

-- combines all GTiles into a dataframe
vertices ∷ [GTile] → DataFrame Vertex '[XN 0]
vertices ts = fromList $ combineVertices ts
vertsqs ∷ [DataFrame Vertex ('[] ∷ [Nat])]
vertsqs = [ S $ Vertex (vec3 (-1) (-1) 0) (vec4 1 0 0 1) (vec3 0 1 0.1) (vec3 0 0 0)
          , S $ Vertex (vec3   1  (-1) 0) (vec4 0 1 0 1) (vec3 1 1 0.1) (vec3 0 0 0)
          , S $ Vertex (vec3   1    1  0) (vec4 0 0 1 1) (vec3 1 0 0.1) (vec3 0 0 0)
          , S $ Vertex (vec3 (-1)   1  0) (vec4 1 1 1 1) (vec3 0 0 0.1) (vec3 0 0 0) ]
combineVertices [] = []
combineVertices ((GTileCached gtdata):tts) = gtdata ⧺ combineVertices tts
combineVertices (tile:tts) = withMove (+ vec3 0 0 m) (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x0 y0 0 0) (withScale (* vec3 xscale yscale 1) vertsqs)))) ⧺ combineVertices tts
  where (x',y') = tPos tile
        (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
        (ax', ay') = tInd tile
        ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
        m = case (tMoves tile) of
              True  → 1.0
              False → 0.0
        (xscale',yscale') = tScale tile
        (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
        (sx, sy) = tSize tile
        t = fromIntegral $ tT tile
withPos f = map (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
withScale f = map (\(S v) → S v { pos = f $ pos v })
withMove f = map (\(S v) → S v { move = f $ move v })

indices ∷ [GTile] → DataFrame Word32 '[XN 3]
indices tiles = atLeastThree $ fromList $ (combineIndices tiles)
combineIndices ∷ ∀ a. (Num a) ⇒ [GTile] → [a]
combineIndices []           = []
combineIndices (_:tiles) = oneRectIndices ⧺ (map (+4) (combineIndices tiles))
  where oneRectIndices = [0,3,2,2,1,0]

