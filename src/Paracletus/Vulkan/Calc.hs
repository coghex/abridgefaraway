module Paracletus.Vulkan.Calc ( calcVertices ) where
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
calcVertices ∷ [GTile] →  (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])
calcVertices ts = (vertices ts, indices ts)--runEval $ do
  --verts ← rpar $ vertices cam ts
  --inds ← rseq $ indices ts
  --rseq verts
  --return (verts,inds)
-- combines all GTiles into a dataframe
-- of vertices, preformed every frame
vertices ∷ [GTile] → DataFrame Vertex '[XN 0]
vertices ts = fromList $ combineVertices ts
  where vertsqs = [ S $ Vertex (vec3 (-1) (-1) 0) (vec4 1 0 0 1) (vec3 0 1 0.1) (vec3 0 0 0)
                  , S $ Vertex (vec3   1  (-1) 0) (vec4 0 1 0 1) (vec3 1 1 0.1) (vec3 0 0 0)
                  , S $ Vertex (vec3   1    1  0) (vec4 0 0 1 1) (vec3 1 0 0.1) (vec3 0 0 0)
                  , S $ Vertex (vec3 (-1)   1  0) (vec4 1 1 1 1) (vec3 0 0 0.1) (vec3 0 0 0) ]
        combineVertices [] = []
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

